(ns uml-viewer.adapters.opencode-spec
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [speclj.core :refer :all]
            [uml-viewer.adapters.opencode :as oc]
            [uml-viewer.domain.companion :as c]
            [uml-viewer.domain.mailbox :as mailbox])
  (:import [java.lang ProcessHandle]
           [com.sun.net.httpserver HttpServer HttpHandler]
           [java.net InetSocketAddress]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util Base64]
           [java.util.concurrent LinkedBlockingQueue]))

(defn temp-root [] (str (Files/createTempDirectory "uv-opencode-" (make-array FileAttribute 0))))

(defn fake-server [handler]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)]
    (.createContext server "/"
      (reify HttpHandler
        (handle [_ exchange]
          (try
            (let [body (slurp (.getRequestBody exchange))
                  request {:method (.getRequestMethod exchange)
                           :path (.getPath (.getRequestURI exchange))
                           :auth (.getFirst (.getRequestHeaders exchange) "Authorization")
                           :upgrade (.getFirst (.getRequestHeaders exchange) "Upgrade")
                           :body (when (seq body) (json/read-str body :key-fn keyword))}
                  [code data] (handler request)
                  bytes (.getBytes (json/write-str data) "UTF-8")]
              (.sendResponseHeaders exchange code (alength bytes))
              (with-open [out (.getResponseBody exchange)] (.write out bytes)))
            (finally (.close exchange))))))
    (.start server)
    {:server server :endpoint (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(describe "OpenCode HTTP adapter"
  (after (when (some-> @oc/!runtime :running) (oc/stop!)) (reset! oc/!runtime nil))
  (it "authenticates requests and redacts server errors"
    (let [got (atom nil)
          {:keys [server endpoint]} (fake-server #(do (reset! got %) [403 {:password "server-secret"}]))]
      (try
        (let [ctx {:endpoint endpoint :password "private-secret"}
              e (try (oc/request! ctx "GET" "/anything" nil) (catch Exception e e))]
          (should= "OpenCode HTTP 403" (.getMessage e))
          (should-be-nil (:upgrade @got))
          (should= (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes "opencode:private-secret" "UTF-8"))) (:auth @got))
          (should-not (re-find #"secret" (pr-str (ex-data e)))))
        (finally (.stop server 0)))))
  (it "rejects unsupported contracts before submitting a prompt"
    (should-not (oc/compatible-contract? {:paths {}}))
    (should (oc/compatible-contract?
              {:paths (reduce (fn [paths [route method]] (assoc-in paths [(keyword route) method] {})) {} oc/required-routes)})))
  (it "verifies permissions and rejects inherited automatic shell grants"
    (let [root "/tmp/project" relative (c/write-paths "examples/p.edn")
          ctx {:root root :paths-relative relative :paths (mapv #(str root "/" %) relative)}
          rules (c/permission-rules root relative)]
      (should (oc/verify-agent! ctx [{:name c/agent-name :permission rules}]))
      (should (oc/verify-agent! ctx [{:name c/agent-name :permission
                                     (into [{:permission "bash" :pattern "git *" :action "allow"}] rules)}]))
      (should-throw clojure.lang.ExceptionInfo
        (oc/verify-agent! ctx [{:name c/agent-name :permission (conj rules {:permission "bash" :pattern "rm *" :action "allow"})}]))))
  (it "persists the notification identity before HTTP submission"
    (let [root (temp-root) calls (atom []) persisted (atom nil)
          {:keys [server endpoint]} (fake-server (fn [req]
            (swap! calls conj req)
            (reset! persisted (get-in (mailbox/read-companion root) [:delivery :message-id]))
            [200 {}]))
          ctx {:backend :opencode :root root :session-id "ses_a" :endpoint endpoint :password "secret"}]
      (try
        (reset! oc/!runtime {:ctx ctx})
        (oc/send-delivery! ctx {:message-id "msg_stable" :ids [1]} "wake")
        (should= "msg_stable" @persisted)
        (should= "msg_stable" (get-in (first @calls) [:body :messageID]))
        (should-not (contains? (mailbox/read-companion root) :password))
        (finally (.stop server 0)))))
  (it "coalesces busy mail and reconciles accepted notifications without resending"
    (let [root (temp-root) posts (atom 0) completed (atom false)
          {:keys [server endpoint]} (fake-server
            (fn [{:keys [method path]}]
              (cond
                (= method "POST") (do (swap! posts inc) [200 {}])
                (.endsWith path "/message/msg_saved") [200 {:info {:id "msg_saved"}}]
                (.endsWith path "/message") [200 (if @completed [{:info {:role "assistant" :parentID "msg_saved" :time {:completed 1}}}] [])]
                :else [404 {}])))
          ctx {:backend :opencode :root root :session-id "ses_a" :endpoint endpoint :password "secret"}
          idle {:busy? false :approvals [] :questions []}]
      (try
        (reset! oc/!runtime {:ctx ctx})
        (mailbox/write-command! (mailbox/to-agent root) :regen {})
        (should= ctx (oc/dispatch-mail! ctx (assoc idle :busy? true)))
        (let [ctx (assoc ctx :delivery {:message-id "msg_saved" :ids [1]})]
          (should= ctx (oc/dispatch-mail! ctx idle))
          (reset! completed true)
          (should-throw clojure.lang.ExceptionInfo (oc/dispatch-mail! ctx idle))
          (mailbox/take-command! (mailbox/to-agent root))
          (should-not (:delivery (oc/dispatch-mail! ctx idle))))
        (should= 0 @posts)
        (finally (.stop server 0)))))
  (it "preserves work and stops on an unreconciled message"
    (let [root (temp-root)
          {:keys [server endpoint]} (fake-server (constantly [404 {}]))
          ctx {:root root :endpoint endpoint :password "secret" :session-id "ses_a"
               :delivery {:message-id "lost" :ids [1]}}]
      (try
        (mailbox/write-command! (mailbox/to-agent root) :regen {})
        (should-throw clojure.lang.ExceptionInfo
          (oc/dispatch-mail! ctx {:busy? false :approvals [] :questions []}))
        (should= 1 (count (:queue (mailbox/read-mailbox (mailbox/to-agent root)))))
        (finally (.stop server 0)))))
  (it "filters permission requests by session and reconciles terminal replies"
    (let [requests (atom [{:id "mine" :sessionID "ses_a"} {:id "other" :sessionID "ses_b"}])
          {:keys [server endpoint]} (fake-server
            (fn [{:keys [path]}]
              [200 (case path "/permission" @requests "/question" [] "/session/status" {} {})]))
          ctx {:endpoint endpoint :password "secret" :session-id "ses_a"}]
      (try
        (reset! oc/!runtime {:ctx ctx :commands (LinkedBlockingQueue.)})
        (oc/poll! ctx)
        (should= ["mine"] (mapv :id (:approvals (oc/snapshot))))
        (oc/reply! "mine" "always")
        (should (contains? (:submitted (oc/snapshot)) "mine"))
        (reset! requests [])
        (oc/poll! ctx)
        (should= [] (:approvals (oc/snapshot)))
        (should= #{} (:submitted (oc/snapshot)))
        (finally (.stop server 0)))))
  (it "does not claim an unrelated live process with a reused PID"
    (let [ctx {:pid (.pid (ProcessHandle/current)) :process-start "wrong"}]
      (should-be-nil (oc/owned-process ctx))
      (oc/stop-process! ctx)
      (should (.isAlive (ProcessHandle/current))))))

(defn await-condition [f]
  (loop [remaining 200]
    (cond (f) true (zero? remaining) false
          :else (do (Thread/sleep 20) (recur (dec remaining))))))

(describe "OpenCode lifecycle with a fake HTTP server"
  (after (when (some-> @oc/!runtime :running) (oc/stop!)) (reset! oc/!runtime nil))
  (it "starts asynchronously, resumes a recorded session, replies, and recovers without replay"
    (let [root (temp-root) calls (atom []) stored (atom {}) ctx-holder (atom nil)
          approvals (atom []) fail-once (atom false) health-failure (atom false) launches (atom 0)
          contract {:paths (reduce (fn [p [route method]] (assoc-in p [route method] {})) {} oc/required-routes)}
          {:keys [server endpoint]} (fake-server
            (fn [{:keys [path method body] :as request}]
              (swap! calls conj (select-keys request [:path :method :body]))
              (cond
                (= path "/global/health") (if (compare-and-set! health-failure true false) [503 {}] [200 {:healthy true}])
                (= path "/doc") [200 contract]
                (= path "/config/providers") [200 {:providers [{:id "test"}]}]
                (= path "/agent") [200 [{:name c/agent-name :permission (c/permission-rules root (:paths-relative @ctx-holder))}]]
                (= path "/session/status") (if (compare-and-set! fail-once true false) (do (reset! health-failure true) [503 {}]) [200 {}])
                (= path "/permission") [200 @approvals]
                (= path "/question") [200 []]
                (= path "/session") [200 {:id "ses_owned"}]
                (= path "/session/ses_owned") [200 {:id "ses_owned"}]
                (= path "/session/ses_owned/prompt_async")
                (do (swap! stored assoc (:messageID body) {:info {:id (:messageID body)}})
                    (while (mailbox/take-command! (mailbox/to-agent root)))
                    [200 {}])
                (= path "/session/ses_owned/message")
                [200 (mapv (fn [id] {:info {:role "assistant" :parentID id :time {:completed 1}}}) (keys @stored))]
                (.startsWith path "/session/ses_owned/message/")
                (if-let [message (get @stored (last (clojure.string/split path #"/")))] [200 message] [404 {}])
                (= path "/permission/p1/reply") (do (reset! approvals []) [200 true])
                :else [200 {}])))]
      (try
        (with-redefs [oc/launch-process! (fn [ctx] (swap! launches inc) (let [ctx (assoc ctx :endpoint endpoint)] (reset! ctx-holder ctx) ctx))]
          (should= :started (oc/start! root (str root "/diagram.edn") "rules" "launch" false))
          (let [ready? (await-condition #(and (= :idle (:status (oc/snapshot)))
                                             (:session-id (:ctx @oc/!runtime))
                                             (nil? (:delivery (:ctx @oc/!runtime)))))]
            (when-not ready? (println "Startup diagnostic:" (oc/snapshot)))
            (should ready?))
          (should= :idle (:status (oc/snapshot)))
          (should= "rw-------" (java.nio.file.attribute.PosixFilePermissions/toString
                                 (Files/getPosixFilePermissions (.toPath (io/file root ".uml-viewer/opencode-auth.edn"))
                                                               (make-array java.nio.file.LinkOption 0))))
          (should-not (re-find #"password" (slurp (mailbox/companion-file root))))
          (mailbox/write-command! (mailbox/to-agent root) :regen {})
          (oc/notify!)
          (should (await-condition #(and (empty? (:queue (mailbox/read-mailbox (mailbox/to-agent root))))
                                         (nil? (:delivery (:ctx @oc/!runtime))))))
          (reset! approvals [{:id "p1" :sessionID "ses_owned" :permission "bash" :patterns ["clj -M:ir"] :always ["clj *"]}])
          (oc/notify!)
          (should (await-condition #(= :approval-needed (:status (oc/snapshot)))))
          (oc/reply! "p1" "always")
          (oc/reply! "p1" "once")
          (should (await-condition #(empty? @approvals)))
          (should= [{:reply "always"}] (mapv :body (filter #(= "/permission/p1/reply" (:path %)) @calls)))
          (reset! fail-once true)
          (oc/notify!)
          (should (await-condition #(and (not @fail-once) (= :idle (:status (oc/snapshot))))))
          (should= 2 @launches)
          (let [post-count (count (filter #(= "/session/ses_owned/prompt_async" (:path %)) @calls))]
            (oc/stop!)
            (should= :started (oc/start! root (str root "/diagram.edn") "rules" "launch" true))
            (should (await-condition #(= :idle (:status (oc/snapshot)))))
            (should= post-count (count (filter #(= "/session/ses_owned/prompt_async" (:path %)) @calls)))
            (should= 1 (count (filter #(= "/session" (:path %)) @calls))))
          (oc/stop!))
        (finally (.stop server 0)))))
  (it "reports missing executables without blocking the caller"
    (let [root (temp-root) running (atom true)
          ctx {:root root :endpoint "http://127.0.0.1:12345" :paths [] :config {} :password "secret"
               :executable "/nonexistent/uml-viewer-opencode"}]
      (reset! oc/!runtime {:running running :ctx ctx :wake (LinkedBlockingQueue.)})
      (should-throw clojure.lang.ExceptionInfo (oc/launch-process! ctx))
      (reset! running false))))

(describe "companion process ownership"
  (it "terminates its owned server while leaving the test JVM alive"
    (let [process (.start (ProcessBuilder. ^java.util.List ["/bin/sh" "-c" "sleep 30"]))
          handle (.toHandle process)
          ctx {:pid (.pid process) :process-start (str (.orElse (.startInstant (.info handle)) nil))}]
      (try
        (should (oc/owned-process ctx))
        (oc/stop-process! ctx)
        (should (await-condition #(not (.isAlive process))))
        (should (.isAlive (ProcessHandle/current)))
        (finally (.destroyForcibly process)))))
  (it "reports absent providers before any launch prompt"
    (let [root "/tmp/provider-check" paths (c/write-paths "diagram.edn")
          ctx {:root root :paths-relative paths :paths (mapv #(str root "/" %) paths)}
          contract {:paths (reduce (fn [p [route method]] (assoc-in p [(keyword route) method] {})) {} oc/required-routes)}]
      (with-redefs [oc/request! (fn [_ _ path _]
                                 (case path
                                   "/global/health" {:healthy true}
                                   "/doc" contract
                                   "/agent" [{:name c/agent-name :permission (c/permission-rules root paths)}]
                                   "/config/providers" {:providers []}))]
        (should-throw clojure.lang.ExceptionInfo #"OpenCode has no configured provider"
                      (oc/validate-server! ctx))))))

(describe "OpenCode-managed tool output permissions"
  (it "accepts the cache grant appended by OpenCode 1.18.34 without allowing other directories"
    (let [root "/tmp/project" relative (c/write-paths "diagram.edn")
          cache "/home/test/.local/share/opencode/tool-output/*"
          ctx {:root root :paths-relative relative :paths (mapv #(str root "/" %) relative)
               :tool-output-pattern cache}
          rules (into [{:permission "*" :pattern "*" :action "allow"}]
                      (c/permission-rules root relative))
          rules (conj rules {:permission "external_directory" :pattern cache :action "allow"})]
      (should (oc/verify-agent! ctx [{:name c/agent-name :permission rules}]))
      (should= "ask" (oc/effective-permission rules "bash" "clj -M:ir"))
      (should= "ask" (oc/effective-permission rules "edit" (str root "/src/example.clj")))
      (should-throw clojure.lang.ExceptionInfo
        (oc/verify-agent! ctx [{:name c/agent-name :permission
                               (conj rules {:permission "external_directory" :pattern "/tmp/unrelated/*" :action "allow"})}]))
      (should-throw clojure.lang.ExceptionInfo
        (oc/verify-agent! ctx [{:name c/agent-name :permission
                               (conj rules {:permission "bash" :pattern "*" :action "allow"})}])))))

(describe "OpenCode generated output handoff"
  (it "queues a display for a newly generated diagram and does not duplicate it"
    (let [root (.toFile (Files/createTempDirectory "uml-display-" (make-array FileAttribute 0)))
          diagram (io/file root "diagram.edn")
          ctx {:root (.getPath root) :diagram (.getPath diagram)}]
      (spit diagram (pr-str {:classes [{:id :sample}] :edges []}))
      (let [updated (#'oc/announce-diagram! ctx)
            again (#'oc/announce-diagram! updated)
            commands (:queue (mailbox/read-mailbox (mailbox/to-viewer root)))]
        (should= [{:id 1 :op :display :path (.getPath diagram)}] commands)
        (should= (:artifact-stamp updated) (:artifact-stamp again))
        (should= 1 (count (:queue (mailbox/read-mailbox (mailbox/to-viewer root)))))))))

(ns uml-viewer.adapters.opencode
  "OpenCode effects. A single worker owns HTTP and process lifecycle per viewer."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [uml-viewer.domain.companion :as companion]
            [uml-viewer.domain.mailbox :as mailbox])
  (:import [java.lang ProcessHandle]
           [java.net URI ServerSocket]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.nio.file Files LinkOption]
           [java.nio.file.attribute PosixFilePermissions FileAttribute]
           [java.util Base64 UUID]
           [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defonce !runtime (atom nil))
;; Bun's OpenCode server can accept an h2c upgrade on GET while stalling POST
;; request bodies. Use HTTP/1.1 consistently, including the event stream.
(def client (-> (HttpClient/newBuilder)
                (.version HttpClient$Version/HTTP_1_1)
                (.connectTimeout (Duration/ofSeconds 3))
                .build))

(defn request!
  "Never include a server body, authorization, or credentials in an exception."
  [ctx method path body]
  (let [auth (.encodeToString (Base64/getEncoder)
                             (.getBytes (str "opencode:" (:password ctx)) "UTF-8"))
        builder (-> (HttpRequest/newBuilder (URI/create (str (:endpoint ctx) path)))
                    (.timeout (Duration/ofSeconds 5))
                    (.header "Authorization" (str "Basic " auth))
                    (.header "Content-Type" "application/json"))
        publisher (if body (HttpRequest$BodyPublishers/ofString (json/write-str body))
                      (HttpRequest$BodyPublishers/noBody))
        response (.send client (.build (.method builder method publisher)) (HttpResponse$BodyHandlers/ofString))
        code (.statusCode response)]
    (when-not (<= 200 code 299)
      (throw (ex-info (str "OpenCode HTTP " code) {:status code})))
    (when-not (str/blank? (.body response))
      (json/read-str (.body response) :key-fn keyword))))

(defn- private-write! [file data]
  (io/make-parents file)
  (let [path (.toPath (io/file file))
        perms (PosixFilePermissions/fromString "rw-------")]
    (when (Files/isSymbolicLink path)
      (throw (ex-info "Companion credential file must not be a symlink" {})))
    (when-not (Files/exists path (make-array LinkOption 0))
      (Files/createFile path (into-array FileAttribute [(PosixFilePermissions/asFileAttribute perms)])))
    (Files/setPosixFilePermissions path perms)
    (spit file (pr-str data))))

(defn- credentials-file [root] (io/file (mailbox/dir root) "opencode-auth.edn"))

(defn- persist! [ctx]
  (mailbox/write-companion! (:root ctx)
    (select-keys ctx [:backend :root :endpoint :session-id :pid :process-start :delivery :executable :diagram])))

(defn owned-process [ctx]
  (when-let [pid (:pid ctx)]
    (let [handle (.orElse (ProcessHandle/of (long pid)) nil)]
      (when (and handle
                 (= (:process-start ctx) (some-> handle .info .startInstant (.orElse nil) str)))
        handle))))

(defn stop-process! [ctx]
  (when-let [handle (owned-process ctx)]
    ;; Descendants belong to this exact server process, not other OpenCode sessions.
    (with-open [children (.descendants handle)]
      (doseq [child (reverse (vec (.toArray children)))] (.destroy ^ProcessHandle child)))
    (.destroy handle)))

(defn- available-port [] (with-open [socket (ServerSocket. 0)] (.getLocalPort socket)))

(defn- launch-process-unlocked! [ctx]
  (let [port (Integer/parseInt (last (str/split (:endpoint ctx) #":")))
        pb (ProcessBuilder. ^java.util.List (companion/serve-args (:executable ctx) port))
        env (.environment pb)]
    (.directory pb (io/file (:root ctx)))
    (.put env "OPENCODE_SERVER_PASSWORD" (:password ctx))
    (.put env "OPENCODE_SERVER_USERNAME" "opencode")
    (.put env "OPENCODE_CONFIG_CONTENT" (json/write-str (:config ctx)))
    (.put env "OPENCODE_PERMISSION" (json/write-str (companion/permissions (:paths ctx))))
    (.put env "OPENCODE_AUTO_SHARE" "false")
    (.put env "OPENCODE_DISABLE_AUTOUPDATE" "true")
    ;; Do not pipe raw server/plugin logs into viewer logs (they can contain credentials).
    (.redirectOutput pb java.lang.ProcessBuilder$Redirect/DISCARD)
    (.redirectError pb java.lang.ProcessBuilder$Redirect/DISCARD)
    (let [p (try (.start pb)
                 (catch java.io.IOException _
                   (throw (ex-info "Cannot launch OpenCode; install it and configure a provider, or set OPENCODE_BIN" {:fatal true}))))
          ctx (assoc ctx :pid (.pid p)
                         :process-start (str (.orElse (.startInstant (.info (.toHandle p))) nil)))]
      (persist! ctx)
      ctx)))

(defn launch-process! [ctx]
  (locking !runtime
    (when-not (some-> @!runtime :running deref)
      (throw (ex-info "Companion stopped" {:fatal true})))
    (let [ctx (launch-process-unlocked! ctx)]
      (swap! !runtime assoc :ctx ctx)
      ctx)))

(def required-routes
  [["/global/health" :get] ["/agent" :get] ["/session" :post]
   ["/session/status" :get] ["/session/{sessionID}" :get]
   ["/session/{sessionID}/prompt_async" :post]
   ["/session/{sessionID}/message/{messageID}" :get]
   ["/session/{sessionID}/message" :get] ["/config/providers" :get]
   ["/permission" :get] ["/permission/{requestID}/reply" :post]
   ["/question" :get] ["/event" :get]])

(defn compatible-contract? [doc]
  (every? (fn [[path method]] (get-in doc [:paths (keyword path) method])) required-routes))

(defn- wildcard-match? [pattern value]
  (boolean (re-matches
             (re-pattern (str "^" (str/join ".*" (map #(java.util.regex.Pattern/quote %) (str/split pattern #"\*" -1))) "$"))
             value)))

(defn effective-permission [rules permission pattern]
  (:action (last (filter #(and (wildcard-match? (:permission %) permission)
                               (wildcard-match? (:pattern %) pattern)) rules))))

(defn verify-agent! [ctx agents]
  (let [agent (first (filter #(= companion/agent-name (:name %)) agents))
        rules (:permission agent)
        expected (companion/permission-rules (:root ctx) (:paths-relative ctx))]
    (when-not (and agent (sequential? rules)
                   (every? #(or (not= "allow" (effective-permission rules (:permission %) (:pattern %)))
                                (contains? #{"read" "glob" "grep" "question"} (:permission %))
                                ;; OpenCode appends access to its own truncated-tool-output cache.
                                ;; This directory guard does not grant edit or shell permissions.
                                (and (= "external_directory" (:permission %))
                                     (= (:tool-output-pattern ctx) (:pattern %)))
                                (and (= "edit" (:permission %))
                                     (contains? (set (:paths ctx)) (:pattern %)))) rules)
                   (every? #(= (:action %) (effective-permission rules (:permission %) (:pattern %))) expected)
                   (= "ask" (effective-permission rules "bash" "echo hello"))
                   (= "ask" (effective-permission rules "edit" (str (:root ctx) "/src/example.clj"))))
      (throw (ex-info "OpenCode companion permissions could not be verified" {:fatal true})))
    true))

(defn validate-server! [ctx]
  (when-not (:healthy (request! ctx "GET" "/global/health" nil))
    (throw (ex-info "OpenCode server is unhealthy" {})))
  (when-not (compatible-contract? (request! ctx "GET" "/doc" nil))
    (throw (ex-info "Installed OpenCode lacks the required session/approval API" {:fatal true})))
  (verify-agent! ctx (request! ctx "GET" "/agent" nil))
  (let [providers (request! ctx "GET" "/config/providers" nil)]
    (when-not (seq (:providers providers))
      (throw (ex-info "OpenCode has no configured provider; configure one in OpenCode first" {:fatal true}))))
  ctx)

(defn- message-id []
  (str "msg_" (format "%012x" (System/currentTimeMillis))
       (subs (str/replace (str (UUID/randomUUID)) "-" "") 0 14)))

(defn- message-path [ctx id]
  (str "/session/" (:session-id ctx) "/message/" id))

(defn- lookup-message! [ctx id]
  (try (request! ctx "GET" (message-path ctx id) nil)
       (catch clojure.lang.ExceptionInfo e
         (if (= 404 (:status (ex-data e))) nil (throw e)))))

(defn send-delivery! [ctx delivery text]
  ;; Persist BEFORE submitting. A lost response can then be reconciled by ID.
  (let [delivery (assoc delivery :created-at (System/currentTimeMillis))
        ctx (assoc (dissoc ctx :recovered?) :delivery delivery)]
    (persist! ctx)
    (swap! !runtime assoc :ctx ctx)
    (request! ctx "POST" (str "/session/" (:session-id ctx) "/prompt_async")
              {:messageID (:message-id delivery) :agent companion/agent-name
               :parts [{:type "text" :text text}]})
    ctx))

(defn- start-session! [ctx prompt]
  (let [session (request! ctx "POST" "/session"
                         {:title "UML viewer companion"
                          :permission (companion/permission-rules (:root ctx) (:paths-relative ctx))})
        ctx (assoc ctx :session-id (:id session))]
    (when-not (seq (:session-id ctx))
      (throw (ex-info "OpenCode did not return a session ID" {:fatal true})))
    (persist! ctx)
    (send-delivery! ctx {:message-id (message-id) :launch? true :ids []} prompt)))

(defn- scoped [requests session] (vec (filter #(= session (:sessionID %)) requests)))

(defn poll! [ctx]
  (let [sid (:session-id ctx)
        statuses (request! ctx "GET" "/session/status" nil)
        busy? (not (contains? #{nil "idle"} (get-in statuses [(keyword sid) :type])))
        approvals (scoped (request! ctx "GET" "/permission" nil) sid)
        questions (scoped (request! ctx "GET" "/question" nil) sid)]
    (swap! !runtime assoc :status (companion/status busy? approvals questions)
           :approvals approvals :questions questions)
    (swap! !runtime update :submitted #(set (filter (set (map :id approvals)) %)))
    {:busy? busy? :approvals approvals :questions questions}))

(defn- completed-message! [ctx id]
  (let [messages (request! ctx "GET" (str "/session/" (:session-id ctx) "/message") nil)]
    (when (some #(and (= id (get-in % [:info :parentID])) (get-in % [:info :error])) messages)
      (throw (ex-info "OpenCode reported a session error; inspect its terminal. Queued work preserved." {:fatal true})))
    (some #(and (= "assistant" (get-in % [:info :role]))
                (= id (get-in % [:info :parentID]))
                (get-in % [:info :time :completed])) messages)))

(defn- artifact-stamp [diagram]
  (let [file (io/file diagram)]
    (when (and file (.isFile file)) [(.lastModified file) (.length file)])))

(defn- announce-diagram! [ctx]
  (let [stamp (artifact-stamp (:diagram ctx))
        doc (when (not= stamp (:artifact-stamp ctx)) (mailbox/read-command (:diagram ctx)))
        valid? (and (map? doc) (or (contains? doc :classes) (contains? doc :diagrams)))
        pending? (some #(and (= :display (:op %)) (= (:diagram ctx) (:path %)))
                       (:queue (mailbox/read-mailbox (mailbox/to-viewer (:root ctx)))))]
    (when (companion/announce-diagram? (:artifact-stamp ctx) stamp valid? pending?)
      (mailbox/write-command! (mailbox/to-viewer (:root ctx)) :display {:path (:diagram ctx)}))
    (if valid? (assoc ctx :artifact-stamp stamp) ctx)))

(defn dispatch-mail! [ctx snapshot]
  (let [queue (:queue (mailbox/read-mailbox (mailbox/to-agent (:root ctx))))
        delivery (:delivery ctx)
        message (when delivery (lookup-message! ctx (:message-id delivery)))
        completed? (when (and delivery message (not (:busy? snapshot)))
                     (completed-message! ctx (:message-id delivery)))
        _ (when (and (:recovered? ctx) delivery message (not (:busy? snapshot))
                     (empty? (:approvals snapshot)) (empty? (:questions snapshot)) (not completed?))
            (throw (ex-info "Interrupted message requires review in the attached terminal; queued work preserved" {:fatal true})))
        outcome (companion/delivery-outcome delivery message (boolean completed?) (map :id queue))]
    (case outcome
      :missing (if (or (:recovered? ctx) (companion/delivery-expired? delivery (System/currentTimeMillis)))
                 (throw (ex-info "A submitted message cannot be reconciled; queued work preserved. Restart explicitly to continue." {:fatal true}))
                 ctx)
      :unhandled (throw (ex-info "Companion finished without handling queued mail; queued work preserved." {:fatal true}))
      :done (let [ctx (dissoc (announce-diagram! ctx) :delivery :recovered?)] (persist! ctx) ctx)
      :none (if (companion/dispatch? (:busy? snapshot) (:approvals snapshot) (:questions snapshot) queue nil)
              (send-delivery! ctx {:message-id (message-id) :ids (mapv :id queue)}
                              "You have mail from the viewer. Read .uml-viewer/to-agent.edn; handle and pop each command in order.")
              ctx)
      ctx)))

(defn- event-stream! [ctx running]
  (future
    (try
      (let [auth (.encodeToString (Base64/getEncoder) (.getBytes (str "opencode:" (:password ctx)) "UTF-8"))
            req (-> (HttpRequest/newBuilder (URI/create (str (:endpoint ctx) "/event")))
                    (.header "Authorization" (str "Basic " auth)) .GET .build)
            response (.send client req (HttpResponse$BodyHandlers/ofInputStream))]
        (with-open [body (.body response) reader (io/reader body)]
          (swap! !runtime assoc :event-body body)
          (while @running
            (if-let [line (.readLine ^java.io.BufferedReader reader)]
              (when (str/starts-with? line "data:")
                (let [event (json/read-str (subs line 5) :key-fn keyword)
                      normalized (companion/normalize-event event (:session-id ctx))]
                  (when (= :error (:kind normalized))
                    (swap! !runtime assoc :session-error? true))))
              (throw (ex-info "Event stream closed" {}))))))
      (catch Exception _ nil))))

(defn reply! [id reply]
  (locking !runtime
    (when-let [runtime @!runtime]
      (when (and (contains? #{"once" "always" "reject"} reply)
                 (not (contains? (:submitted runtime) id))
                 (some #(= id (:id %)) (:approvals runtime)))
        (swap! !runtime update :submitted (fnil conj #{}) id)
        (.offer ^LinkedBlockingQueue (:commands runtime) {:id id :reply reply})))))

(defn- handle-replies! [ctx]
  (let [commands (:commands @!runtime)]
    (when-let [cmd (.poll ^LinkedBlockingQueue commands)]
      ;; Reconcile first: a terminal may already have answered this request.
      (when (some #(= (:id cmd) (:id %)) (:approvals @!runtime))
        (try
          (request! ctx "POST" (str "/permission/" (:id cmd) "/reply") {:reply (:reply cmd)})
          (catch Exception e
            (swap! !runtime update :submitted disj (:id cmd))
            (throw e)))))))

(defn- safe-error [e]
  (cond
    (instance? clojure.lang.ExceptionInfo e) (.getMessage e)
    (instance? java.net.http.HttpTimeoutException e) "OpenCode request timed out"
    :else "OpenCode connection failed"))

(defn- recover! [ctx running]
  (loop [attempt 0 ctx ctx]
    (when @running
      (try
        (request! ctx "GET" "/global/health" nil)
        (catch Exception _
          (when-not (owned-process ctx)
            (swap! !runtime assoc :ctx (launch-process! ctx))))))
    (let [ctx (or (:ctx @!runtime) ctx)
          result (try
                   (validate-server! ctx)
                   (when (:session-id ctx)
                     (try (request! ctx "GET" (str "/session/" (:session-id ctx)) nil)
                          (catch clojure.lang.ExceptionInfo e
                            (if (= 404 (:status (ex-data e)))
                              (throw (ex-info "Recorded OpenCode session is unavailable; queued work preserved" {:fatal true}))
                              (throw e)))))
                   {:ctx ctx}
                   (catch Exception e {:error e}))]
      (cond
        (:ctx result) (:ctx result)
        (:fatal (ex-data (:error result))) (throw (:error result))
        (not @running) nil
        :else (do (swap! !runtime assoc :status :recovering :error (safe-error (:error result)))
                  (.poll ^LinkedBlockingQueue (:wake @!runtime) (companion/backoff-ms attempt) TimeUnit/MILLISECONDS)
                  (recur (inc attempt) ctx))))))

(defn- worker! [initial prompt restart? running]
  (try
    (let [ctx (if restart? initial (launch-process! initial))]
      (swap! !runtime assoc :ctx ctx)
      (loop [ctx (cond-> (recover! ctx running) restart? (assoc :recovered? true)) started? restart? stream nil]
        (when (and @running ctx)
          (let [ctx (if started? ctx (start-session! ctx prompt))
                stream (or stream (event-stream! ctx running))
                result (try
                         (when (:session-error? @!runtime)
                           (throw (ex-info "OpenCode session failed; inspect the attached terminal. Queued work preserved." {:fatal true})))
                         (let [snapshot (poll! ctx)]
                           (handle-replies! ctx)
                           {:ctx (dispatch-mail! ctx snapshot)})
                         (catch Exception e {:error e}))]
            (if-let [e (:error result)]
              (if (:fatal (ex-data e)) (throw e)
                  (do (when-let [body (:event-body @!runtime)] (.close ^java.io.Closeable body))
                      (future-cancel stream)
                      (swap! !runtime assoc :status :recovering :error (safe-error e))
                      (recur (assoc (recover! (or (:ctx @!runtime) ctx) running) :recovered? true) true nil)))
              (do (swap! !runtime assoc :ctx (:ctx result) :error nil)
                  (.poll ^LinkedBlockingQueue (:wake @!runtime) 1000 TimeUnit/MILLISECONDS)
                  (recur (:ctx result) true stream)))))))
    (catch Exception e
      (when-let [body (:event-body @!runtime)]
        (try (.close ^java.io.Closeable body) (catch Exception _ nil)))
      (when @running
        (if (and (not (:fatal (ex-data e))) (:session-id (:ctx @!runtime)))
          (do (swap! !runtime assoc :status :recovering :error (safe-error e))
              (worker! (:ctx @!runtime) prompt true running))
          (swap! !runtime assoc :status :failed :error (safe-error e)))))))

(defn- inline-config []
  (if-let [raw (System/getenv "OPENCODE_CONFIG_CONTENT")]
    (try
      (let [config (json/read-str raw)]
        (when-not (map? config) (throw (Exception.)))
        config)
      (catch Exception _ (throw (ex-info "OPENCODE_CONFIG_CONTENT must be a JSON object" {}))))
    {}))

(defn tool-output-pattern
  "Action: resolve the same data directory used by OpenCode on Linux/macOS."
  []
  (str (.getAbsolutePath
         (io/file (or (System/getenv "XDG_DATA_HOME")
                      (str (System/getenv "HOME") "/.local/share"))
                  "opencode" "tool-output")) "/*"))

(defn start! [root diagram rules prompt restart?]
  (let [root (.getCanonicalPath (io/file root))
        diagram (.getCanonicalPath (io/file diagram))
        rel (str (.relativize (.toPath (io/file root)) (.toPath (io/file diagram))))
        _ (when (str/starts-with? rel "..")
            (throw (ex-info "Diagram must be inside the examined project" {})))
        policy (or (:policy-file (mailbox/read-command diagram))
                   (str/replace diagram #"\.edn$" ".policy.edn"))
        classpath (str/join java.io.File/pathSeparator
                           (map #(.getCanonicalPath (io/file %))
                                (str/split (System/getProperty "java.class.path")
                                           (re-pattern java.io.File/pathSeparator))))
        rules (companion/project-instructions rules
                (str (System/getProperty "java.home") "/bin/java") classpath policy diagram)
        paths-relative (companion/write-paths rel)
        paths (mapv #(str root "/" %) paths-relative)
        recorded (when restart? (mailbox/read-companion root))
        password (if restart? (:password (mailbox/read-command (credentials-file root)))
                     (str (UUID/randomUUID) (UUID/randomUUID)))
        _ (when-not (seq password) (throw (ex-info "Companion authentication file is missing" {})))
        ctx (merge {:backend :opencode :root root :diagram diagram :artifact-stamp (artifact-stamp diagram)
                    :endpoint (str "http://127.0.0.1:" (available-port))
                    :executable (or (System/getenv "OPENCODE_BIN") "opencode")}
                   recorded
                   {:password password :paths paths :paths-relative paths-relative
                    :tool-output-pattern (tool-output-pattern)
                    :config (companion/runtime-config rules paths (inline-config))})
        running (atom true)]
    (when-not restart? (private-write! (credentials-file root) {:password password}))
    (reset! !runtime {:ctx ctx :running running :status :starting :approvals [] :questions []
                     :commands (LinkedBlockingQueue.) :wake (LinkedBlockingQueue. 1)})
    (let [worker (future (worker! ctx prompt restart? running))]
      (swap! !runtime assoc :worker worker))
    :started))

(defn notify! []
  (when-let [runtime @!runtime]
    (.offer ^LinkedBlockingQueue (:wake runtime) true)
    (not= :failed (:status runtime))))

(defn snapshot []
  (when-let [runtime @!runtime]
    (select-keys runtime [:status :error :approvals :questions :submitted])))

(defn attach-command []
  (let [ctx (:ctx @!runtime)]
    (when (:session-id ctx)
      (companion/attach-command (:executable ctx) (:endpoint ctx) (:session-id ctx) (:root ctx)))))

(defn password [] (:password (:ctx @!runtime)))

(defn stop! []
  (locking !runtime
    (when-let [runtime @!runtime]
      (reset! (:running runtime) false)
      (.offer ^LinkedBlockingQueue (:wake runtime) true)
      (when-let [body (:event-body runtime)] (.close ^java.io.Closeable body))
      (when-let [worker (:worker runtime)] (future-cancel worker))
      (stop-process! (:ctx runtime))
      (reset! !runtime nil))))

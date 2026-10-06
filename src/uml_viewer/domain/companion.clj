(ns uml-viewer.domain.companion
  "Calculations and data for companion selection, permissions, and delivery."
  (:require [clojure.string :as str]))

(def agent-name "uml-viewer-companion")
(def statuses #{:starting :working :idle :approval-needed :attention-needed :recovering :failed})

(defn backend [record] (or (:backend record) :grok))

(defn select-backend [requested restart? record]
  (if restart?
    (let [saved (when record (backend record))]
      (when-not saved (throw (ex-info "No recorded companion to restart" {})))
      (when (and requested (not= requested saved))
        (throw (ex-info "--agent conflicts with the recorded companion" {})))
      saved)
    (or requested :opencode)))

(defn write-paths [diagram]
  (vec (mapcat #(vector % (str % ".tmp"))
               [".uml-viewer/to-agent.edn" ".uml-viewer/to-viewer.edn" diagram])))

(defn permissions [paths]
  (array-map "*" "ask" "read" {"*" "allow" "*.env" "deny" "*.env.*" "deny"
                         "*.env.example" "allow"}
   "glob" "allow" "grep" "allow" "question" "allow"
   "edit" (into {"*" "ask"} (map #(vector % "allow") paths))
   "bash" "ask" "external_directory" "ask" "task" "ask"))

(defn runtime-config
  ([rules paths] (runtime-config rules paths {}))
  ([rules paths original]
   (let [p (permissions paths)
         agent (merge (get-in original ["agent" agent-name])
                      {"description" "UML viewer companion" "mode" "primary"
                       "prompt" rules "permission" p})]
     (merge original
            {"default_agent" agent-name "share" "disabled" "autoupdate" false
             "permission" p
             "agent" (assoc (get original "agent" {}) agent-name agent)}))))

(defn permission-rules [root paths]
  (into [{:permission "*" :pattern "*" :action "ask"}
         {:permission "read" :pattern "*" :action "allow"}
         {:permission "read" :pattern "*.env" :action "deny"}
         {:permission "read" :pattern "*.env.*" :action "deny"}
         {:permission "read" :pattern "*.env.example" :action "allow"}
         {:permission "glob" :pattern "*" :action "allow"}
         {:permission "grep" :pattern "*" :action "allow"}
         {:permission "question" :pattern "*" :action "allow"}]
        (map #(hash-map :permission "edit" :pattern (str root "/" %) :action "allow") paths)))

(defn serve-args [executable port]
  [executable "serve" "--hostname" "127.0.0.1" "--port" (str port)])

(defn shell-quote [s] (str "'" (str/replace (str s) "'" "'\"'\"'") "'"))

(defn attach-command [executable endpoint session root]
  (str (shell-quote executable) " attach " (shell-quote endpoint)
       " --session " (shell-quote session) " --dir " (shell-quote root)))

(defn backoff-ms [attempt] (long (min 30000 (* 1000 (Math/pow 2 (min 5 attempt))))))

(defn normalize-event [event session]
  (let [e (or (:payload event) event)
        p (:properties e)]
    (when (= session (:sessionID p))
      (case (:type e)
        "permission.asked" {:kind :permission :request p}
        "permission.replied" {:kind :permission-resolved :id (:requestID p)}
        "question.asked" {:kind :question :request p}
        "question.replied" {:kind :question-resolved :id (:requestID p)}
        "question.rejected" {:kind :question-resolved :id (:requestID p)}
        "session.error" {:kind :error}
        nil))))

(defn status [busy? approvals questions]
  (cond (seq approvals) :approval-needed
        (seq questions) :attention-needed
        busy? :working
        :else :idle))

(defn dispatch? [busy? approvals questions queue delivery]
  (and (not busy?) (empty? approvals) (empty? questions)
       (seq queue) (nil? delivery)))

(defn delivery-outcome [delivery message idle? remaining-ids]
  (cond
    (nil? delivery) :none
    (nil? message) :missing
    (not idle?) :waiting
    (and (not (:launch? delivery)) (some (set (:ids delivery)) remaining-ids)) :unhandled
    :else :done))

(defn delivery-expired? [delivery now]
  (or (nil? (:created-at delivery))
      (>= (- now (:created-at delivery)) 10000)))

(defn opencode-instructions [rules]
  (-> rules
      (str/replace "A tmux wake-up" "An HTTP message")
      (str/replace "Do not send tmux yourself." "Do not start another server yourself.")
      (str/replace "Closing the viewer\nkills only this companion's tmux session, not other Grok agents. If\nthis Grok process dies, tmux respawns it in the same pane."
                   "Closing the viewer stops only its owned OpenCode server. The viewer owns server recovery.")))

(defn regen-message [backend woke? status]
  (if woke?
    "Regen requested."
    (if (= :opencode backend)
      (str "Regen queued; OpenCode companion "
           (case status :failed "failed" :recovering "recovering" "unavailable") ".")
      "Regen queued; Grok session not attached.")))

(defn approval-enabled? [status request reply]
  (and (not (contains? #{:starting :failed :recovering} (:status status)))
       (not (contains? (:submitted status) (:id request)))
       (or (not= "always" reply) (seq (:always request)))))

(defn generator-command [java classpath policy diagram]
  (str/join " " (map shell-quote
                     [java "-cp" classpath "clojure.main" "-m"
                      "uml-viewer.main.ir-generator" policy diagram])))

(defn project-instructions [rules java classpath policy diagram]
  (str rules "\nProject-specific generator setup (takes precedence over generic alias examples):\n"
       "Policy file: " (pr-str policy) ". Diagram output: " (pr-str diagram) ".\n"
       "Inspect source, create missing parent directories for the policy/output, and create/update this policy before generation. Minimal policy is EDN "
       "{:title \"Project\" :src \"src\" :prefix \"actual.namespace.prefix\" "
       ":lang :clojure :hierarchical true :out " (pr-str diagram) "}. "
       "Choose the actual source prefix and language; preserve existing policy settings and proposals.\n"
       "Run this exact generator command from the examined project directory; it uses the viewer's "
       "loaded dependencies and does not require a project :ir alias:\n"
       (generator-command java classpath policy diagram) "\n"
       "On launch generate the diagram and enqueue {:id next-id :op :display :path "
       (pr-str diagram) "} in .uml-viewer/to-viewer.edn, incrementing :next-id.\n"
       "On mailbox wake-up perform the commands using tools, not a textual summary. "
       "For :context remember the selection and pop the command immediately. "
       "For :regen run the generator above, enqueue :display, then pop the handled command. "
       "Keep :next-id unchanged when popping; rewrite each queue atomically through its .tmp file. "
       "Do not stop to ask whether to inspect configuration; inspect it with the read tool. "
       "Use the question tool when human input is required. If generation fails, explain the "
       "specific failure and leave unhandled commands queued.\n"))

(defn announce-diagram? [previous current valid? pending?]
  (and valid? (some? current) (not= previous current) (not pending?)))

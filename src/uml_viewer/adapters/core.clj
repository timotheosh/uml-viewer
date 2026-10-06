(ns uml-viewer.adapters.core
  (:require [uml-viewer.adapters.sketch :as sketch]))

(def help-text
  (str "Usage: clj -M:run [options] [edn-file]\n"
       "\n"
       "  edn-file          Diagram to watch (default: examples/library.edn).\n"
       "                    A fresh start waits for the companion to send\n"
       "                    :display unless the associated agent recycles\n"
       "                    the window with :uml-viewer-restart, or you press R.\n"
       "\n"
       "  --restart         Associated agent only (via :uml-viewer-restart).\n"
       "                    New JVM, keep the existing companion session.\n"
       "                    Reloads the last view (depth, pan, zoom, proposal).\n"
       "                    Do not use this if no companion is attached.\n"
       "\n"
       "  --agent NAME      opencode (default) or grok. Restart preserves the agent.\n"
       "\n"
       "  -h, --help        Print this help and exit.\n"))

(defn parse-args
  "Calculate validated CLI options; nil agent means use default or recorded backend."
  [args]
  (loop [args (seq (keep identity args)) result {:help? false :restart? false :path "examples/library.edn" :agent nil} path? false]
    (if-let [arg (first args)]
      (case arg
        ("--help" "-h") (recur (next args) (assoc result :help? true) path?)
        "--restart" (recur (next args) (assoc result :restart? true) path?)
        "--agent" (let [agent (second args)]
                    (when-not (#{"opencode" "grok"} agent)
                      (throw (ex-info "--agent must be opencode or grok" {})))
                    (recur (nnext args) (assoc result :agent (keyword agent)) path?))
        (do (when (or path? (.startsWith ^String arg "--"))
              (throw (ex-info "Unknown argument or multiple diagram paths" {})))
            (recur (next args) (assoc result :path arg) true)))
      result)))

(defn start!
  "Launch the viewer. `source-impl` satisfies `LanguageSource`."
  [source-impl & args]
  (let [{:keys [path restart? help? agent]} (parse-args args)]
    (if help?
      (do (print help-text) :help)
      (do
        (sketch/start! path source-impl restart? agent)
        (println "Watching" path)
        (println "Double-click a class for its card. Scroll to pan (Shift-scroll for horizontal). Ctrl+/− zoom; Ctrl+0 resets. R reloads. Click the real diagram above Proposals, or a proposal to show it.")))))

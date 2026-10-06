(ns uml-viewer.domain.companion-spec
  (:require [speclj.core :refer :all]
            [uml-viewer.domain.companion :as c]))

(describe "companion calculations"
  (it "defaults to OpenCode and interprets legacy records as Grok"
    (should= :opencode (c/select-backend nil false nil))
    (should= :grok (c/select-backend nil true {:session "old"}))
    (should= :opencode (c/select-backend nil true {:backend :opencode}))
    (should-throw clojure.lang.ExceptionInfo (c/select-backend :grok true {:backend :opencode}))
    (should-throw clojure.lang.ExceptionInfo (c/select-backend nil true nil)))
  (it "permits exact output and mailbox files, never the entire runtime directory"
    (let [paths (c/write-paths "examples/project.edn") p (c/permissions paths)]
      (should= "*" (first (keys p)))
      (should= "ask" (get p "bash"))
      (should= "ask" (get-in p ["edit" "*"]))
      (should= "allow" (get-in p ["edit" "examples/project.edn.tmp"]))
      (should= "allow" (get-in p ["edit" ".uml-viewer/to-agent.edn"]))
      (should-not (contains? (get p "edit") ".uml-viewer/*"))
      (should-not (contains? (get p "edit") ".uml-viewer/opencode-auth.edn"))))
  (it "coalesces notifications until idle and approval-free"
    (should (c/dispatch? false [] [] [{:id 1}] nil))
    (should-not (c/dispatch? true [] [] [{:id 1}] nil))
    (should-not (c/dispatch? false [{:id "p"}] [] [{:id 1}] nil))
    (should-not (c/dispatch? false [] [{:id "q"}] [{:id 1}] nil))
    (should-not (c/dispatch? false [] [] [{:id 1}] {:message-id "m"})))
  (it "reconciles accepted messages without treating unfinished work as complete"
    (should= :missing (c/delivery-outcome {:ids [1]} nil true [1]))
    (should= :waiting (c/delivery-outcome {:ids [1]} {} false [1]))
    (should= :unhandled (c/delivery-outcome {:ids [1]} {} true [1]))
    (should= :done (c/delivery-outcome {:ids [1]} {} true [])))
  (it "prioritizes approvals and questions over busy state"
    (should= :approval-needed (c/status true [{}] []))
    (should= :attention-needed (c/status true [] [{}]))
    (should= :working (c/status true [] []))
    (should= :idle (c/status false [] [])))
  (it "filters events from other sessions"
    (should-be-nil (c/normalize-event {:type "permission.asked" :properties {:sessionID "other"}} "ours"))
    (should= {:kind :permission-resolved :id "p"}
             (c/normalize-event {:type "permission.replied" :properties {:sessionID "ours" :requestID "p"}} "ours")))
  (it "caps retries and quotes attach commands without passwords"
    (should= 1000 (c/backoff-ms 0))
    (should= 30000 (c/backoff-ms 99))
    (should= "'opencode' attach 'http://127.0.0.1:4096' --session 'ses_1' --dir '/tmp/a b'"
             (c/attach-command "opencode" "http://127.0.0.1:4096" "ses_1" "/tmp/a b"))))

(describe "delayed notification acceptance"
  (it "allows a short acceptance window without replaying the notification"
    (should-not (c/delivery-expired? {:created-at 1000} 10999))
    (should (c/delivery-expired? {:created-at 1000} 11000))))

(describe "user OpenCode configuration"
  (it "preserves the selected model and integrations while overriding companion effects"
    (let [config (c/runtime-config "rules" ["diagram.edn"]
                   {"model" "provider/model" "mcp" {"local" {"enabled" true}}
                    "agent" {"other" {"description" "user agent"}
                             c/agent-name {"model" "provider/special" "permission" "allow"}}})]
      (should= "provider/model" (get config "model"))
      (should= true (get-in config ["mcp" "local" "enabled"]))
      (should= "user agent" (get-in config ["agent" "other" "description"]))
      (should= "provider/special" (get-in config ["agent" c/agent-name "model"]))
      (should= "ask" (get-in config ["agent" c/agent-name "permission" "bash"])))))

(describe "project generator instructions"
  (it "uses explicit dependencies and paths without requiring project aliases"
    (should= "'/java' '-cp' '/viewer/src:/deps/a.jar' 'clojure.main' '-m' 'uml-viewer.main.ir-generator' '/project/a b.policy.edn' '/project/a b.edn'"
             (c/generator-command "/java" "/viewer/src:/deps/a.jar" "/project/a b.policy.edn" "/project/a b.edn"))
    (let [rules (c/project-instructions "base" "/java" "/cp" "policy.edn" "diagram.edn")]
      (should-contain "does not require a project :ir alias" rules)
      (should-contain "pop the command immediately" rules)
      (should-contain "Use the question tool" rules))))

(describe "generated diagram announcement"
  (it "announces a new valid artifact while avoiding stale, incomplete, or queued duplicates"
    (should (c/announce-diagram? nil [10 100] true false))
    (should (c/announce-diagram? [10 100] [11 100] true false))
    (should-not (c/announce-diagram? [10 100] [10 100] true false))
    (should-not (c/announce-diagram? nil nil true false))
    (should-not (c/announce-diagram? nil [10 100] false false))
    (should-not (c/announce-diagram? nil [10 100] true true))))

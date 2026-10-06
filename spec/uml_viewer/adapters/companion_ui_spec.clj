(ns uml-viewer.adapters.companion-ui-spec
  (:require [speclj.core :refer :all]
            [uml-viewer.adapters.companion-ui :as ui]
            [uml-viewer.adapters.opencode :as oc])
  (:import [java.awt Container]
           [javax.swing JButton JLabel JScrollPane SwingUtilities]))

(defn descendants [component]
  (tree-seq #(instance? Container %) #(seq (.getComponents ^Container %)) component))

(defn buttons [scroll]
  (into {} (for [x (descendants scroll) :when (instance? JButton x)] [(.getText ^JButton x) x])))

(describe "live companion dialog"
  (it "refreshes an already-open dialog when startup finishes and an approval arrives"
    (let [checks (atom [])]
      (SwingUtilities/invokeAndWait
      (fn []
        (let [scroll (JScrollPane.) previous (atom nil) replies (atom [])
              request {:id "p" :permission "bash" :patterns ["printf smoke"] :always ["printf *"]}]
          (ui/refresh! scroll previous {:status :recovering :error "Earlier connection failure"} false)
          (swap! checks conj (not (.isEnabled ^JButton (get (buttons scroll) "Copy attach command"))))
          (ui/refresh! scroll previous {:status :approval-needed :approvals [request]} true)
          (swap! checks conj (.isEnabled ^JButton (get (buttons scroll) "Copy attach command")))
          (swap! checks conj (.isEnabled ^JButton (get (buttons scroll) "Approve once")))
          (with-redefs [oc/reply! (fn [id reply] (swap! replies conj [id reply]))]
            (.doClick ^JButton (get (buttons scroll) "Approve once")))
          (swap! checks conj (= [["p" "once"]] @replies))
          (ui/refresh! scroll previous {:status :approval-needed :approvals [request] :submitted #{"p"}} true)
          (swap! checks conj (not (.isEnabled ^JButton (get (buttons scroll) "Approve once"))))
          (ui/refresh! scroll previous {:status :idle :approvals []} true)
          (swap! checks conj (not (contains? (buttons scroll) "Approve once")))
          (swap! checks conj (some #(and (instance? JLabel %) (= "Status: idle" (.getText ^JLabel %))) (descendants scroll))))))
      (should (every? true? @checks)))))

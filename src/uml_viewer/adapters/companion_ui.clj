(ns uml-viewer.adapters.companion-ui
  "Swing actions for OpenCode approval review and terminal attachment."
  (:require [clojure.string :as str]
            [uml-viewer.adapters.opencode :as opencode]
            [uml-viewer.domain.companion :as companion])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer StringSelection]
           [javax.swing JButton JDialog JPanel JScrollPane JTextArea BoxLayout Timer JLabel]
           [java.awt.event ActionListener]))

(defn copy! [text]
  (when text
    (.setContents (.getSystemClipboard (Toolkit/getDefaultToolkit)) (StringSelection. text) nil)))

(defn request-text [request]
  (str "Operation: " (:permission request) "\n\nTargets:\n"
       (str/join "\n" (:patterns request))
       "\n\nApprove for session grants these exact patterns:\n"
       (str/join "\n" (:always request))
       "\n\nDetails:\n" (pr-str (:metadata request))))

(defn- button [label enabled? action]
  (doto (JButton. label)
    (.setEnabled (boolean enabled?))
    (.addActionListener (reify ActionListener (actionPerformed [_ _] (action))))))

(defn panel
  "Action: render one current snapshot, with callbacks checked against live state."
  [status attached?]
  (let [panel (JPanel.)]
    (.setLayout panel (BoxLayout. panel BoxLayout/Y_AXIS))
    (.add panel (JLabel. (str "Status: " (name (or (:status status) :starting)))))
    (when-let [error (:error status)]
      (.add panel (doto (JTextArea. error) (.setEditable false) (.setLineWrap true) (.setWrapStyleWord true))))
    (when (seq (:questions status))
      (.add panel (JLabel. "OpenCode has a question. Attach the terminal to answer it.")))
    (doseq [request (:approvals status)]
      (let [area (doto (JTextArea. (request-text request) 10 65)
                   (.setEditable false) (.setLineWrap true) (.setWrapStyleWord true))
            buttons (JPanel.)]
        (.add panel (JScrollPane. area))
        (doseq [[label reply] [["Approve once" "once"] ["Approve for session" "always"] ["Reject" "reject"]]]
          (.add buttons
                (button label (companion/approval-enabled? status request reply)
                        #(opencode/reply! (:id request) reply))))
        (.add panel buttons)))
    (let [buttons (JPanel.)]
      (doseq [[label value] [["Copy attach command" opencode/attach-command]
                            ["Copy authentication" opencode/password]]]
        (.add buttons (button label attached? #(copy! (value)))))
      (.add panel buttons))
    (.add panel (JLabel. "Set OPENCODE_SERVER_PASSWORD from the copied authentication before attaching."))
    panel))

(defn refresh!
  "Action: update an existing scroll pane when the snapshot changes. No network I/O."
  [scroll previous status attached?]
  (let [model [status attached?]]
    (when (not= model @previous)
      (reset! previous model)
      (let [position (.getViewPosition (.getViewport ^JScrollPane scroll))]
        (.setViewportView ^JScrollPane scroll (panel status attached?))
        (.setViewPosition (.getViewport ^JScrollPane scroll) position))
      (.revalidate ^JScrollPane scroll)
      (.repaint ^JScrollPane scroll))))

(defn review! []
  (let [dialog (JDialog.)
        scroll (JScrollPane.)
        previous (atom nil)
        refresh (fn [] (refresh! scroll previous (opencode/snapshot) (boolean (opencode/attach-command))))
        timer (Timer. 250 (reify ActionListener (actionPerformed [_ _] (refresh))))]
    (.setTitle dialog "OpenCode companion")
    (refresh)
    (.setContentPane dialog scroll)
    (.setSize dialog 760 560)
    (.setLocationRelativeTo dialog nil)
    (.addWindowListener dialog
      (proxy [java.awt.event.WindowAdapter] []
        (windowClosed [_] (.stop timer))))
    (.setDefaultCloseOperation dialog JDialog/DISPOSE_ON_CLOSE)
    (.setVisible dialog true)
    (.start timer)))

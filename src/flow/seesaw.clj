(ns flow.seesaw
  (:use flow.graphics
        flow.flow2quickfill
        flow.reader)
  (:use seesaw.core
        seesaw.mig
        seesaw.chooser))

(defn on-thread
  [f]
  (.start (Thread. f)))

(def in-file (text :text "screenshot.png" :columns 15))

(def resolution-num (text :text "5" :columns 3))

(defn browse-file
  []
  (choose-file :filters [["Images" ["png" "jpeg"]]]
               :success-fn (fn [fc file] (text! in-file (.getAbsolutePath file)))
               ;:remember-directory? true
               :dir "."
               ))

(def browse-button (button :text "..."
                           :listen [:action (fn [x] (browse-file))]))

(defn go
  [filename board-size]
  (let [board (file->grid filename board-size board-size)]
    (solve-flow-graphic board)))

(def go-button (button :text "Go"
                       :listen [:action (fn [x] (on-thread
                                                  #(go (text in-file)
                                                       (Integer/parseInt
                                                         (text resolution-num)))))]))

(def mig-content
  [["Enter a filename to a screenshot from your phone:" "span, wrap"]
   [in-file]
   [browse-button "wrap"]
   ["Enter the board size of the flow puzzle (e.g. \"5\")" "span, wrap"]
   [resolution-num "wrap"]
   [go-button "span, align center"]])

(defn -main
  [& args]
  (let [m (mig-panel :items mig-content)
        f (frame :content m)]
    (show! (pack! f))))
(-main)
(ns flow.seesaw
  (:use flow.graphics
        flow.flowcore
        flow.reader)
  (:use seesaw.core
        seesaw.mig
        seesaw.chooser)
  (:require [clojure.string :as string]))

(defn on-thread
  [f]
  (.start (Thread. f)))

(def input-board (text :multi-line? true
                       :text "A*B*D\n**C*E\n*****\n*B*D*\n*ACE*"
                       :minimum-size [500 :by 200]))

(def in-file (text :text "screenshot.png" :columns 15))

(def resolution-num (text :text "5" :columns 3))

(defn read-the-image
  []
  (let [n (Integer/parseInt (text resolution-num))
        filename (text in-file)
        grid (try (file->grid filename n n)
               (catch Exception e nil))]
    (if grid
      (text! input-board (clojure.string/join "\n"
                                              (map #(apply str %) grid))))))

(def read-button (button :text "Interpret Image"
                         :listen [:action (fn [x] (read-the-image))]))

(defn browse-file
  []
  (choose-file :filters [["Images" ["png" "jpeg"]]]
               :success-fn (fn [fc file] (text! in-file (.getAbsolutePath file)))
               ;:remember-directory? true
               :dir "."
               ))

(def browse-button (button :text "..."
                           :listen [:action (fn [x] (browse-file))]))

(defn get-board-array
  []
  (->> (text input-board)
    string/split-lines
    (map string/lower-case)
    (map string/trim)
    (filter #(not (= % "")))
    (map #(vec (string/replace % " " "*")))
    vec))

(defn go
  []
  (solve-flow-graphic (get-board-array)))

(def go-button (button :text "Go"
                       :listen [:action (fn [x] (on-thread
                                                  #(go)))]))

(def mig-content
  [["Option 1: Turn on flow labels, and type in the board with characters representing cells." "span, wrap"]
   ["(for an empty space, use a space, or an asterisk.)" "span, wrap"]
   [input-board "span, wrap"]
   ["Option 2: Enter a filename to a screenshot from your phone:" "span, wrap"]
   [in-file]
   [browse-button "wrap"]
   ["and enter the board size of the flow puzzle (e.g. \"5\")" "span, wrap"]
   [resolution-num "wrap"]
   [read-button "wrap"]
   [go-button "span, align center"]])

(defn -main
  [& args]
  (let [m (mig-panel :items mig-content)
        f (frame :content m)]
    (show! (pack! f))))
(-main)
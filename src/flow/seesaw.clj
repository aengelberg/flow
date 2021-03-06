(ns flow.seesaw
  (:use flow.graphics
        flow.flowcore.core
        flow.flowcore.base
        flow.reader)
  (:use seesaw.core
        seesaw.mig
        seesaw.chooser)
  (:require [clojure.string :as string])
  (:import java.util.GregorianCalendar)
  (:gen-class))

(def ^:dynamic *compile-gui*
  "Change this to false when NOT compiling the program into a jar."
  true)

(defmacro maybe
  "Evaluates expr, or returns val (or nil by default) if an error occurs."
  ([expr]
    `(try ~expr
       (catch Exception e# nil)))
  ([expr val]
    `(try ~expr
       (catch Exception e# ~val))))

(defn check-grid
  "Sanity-checks the grid before solving. Returns the grid, or false."
  [grid]
  (and (apply = (map count grid))
       (not (= (count grid) 0))
       (not (= (count (first grid)) 0))
       (let [cpt (color-posn-table grid)]
         (every? (fn [[k v]] (= (count v) 2))
                 cpt))
       grid))

(defmacro time-val
  "Like the macro 'time,' but instead RETURNS the time it took, in milliseconds."
  [expr]
  `(let [x# (.getTimeInMillis (GregorianCalendar.))
         z# ~expr
         y# (.getTimeInMillis (GregorianCalendar.))]
     (- y# x#)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defn failure-message
  [message]
  (show! (pack! (dialog :type :error
                        :content message))))

(defn success-message
  [message]
  (show! (pack! (dialog :type :info
                        :content message))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(def input-board (text :multi-line? true
                       :text "A*B*D\n**C*E\n*****\n*B*D*\n*ACE*"
                       :minimum-size [500 :by 200]))

(def in-file (text :text "screenshot.png" :columns 15))

(def ads? (combobox :model ["Yes" "No"]))

(def resolution-num (text :text "5" :columns 3))

(defn read-the-image
  "The callback function for the Interpret Image button"
  []
  (let [n (maybe (Integer/parseInt (text resolution-num)))]
    (if n
      (let [ads? (case (value ads?)
                   "Yes" true
                   "No" false)
            filename (text in-file)
            grid (maybe (file->grid filename n
                                    :ads? ads?) nil)]
        (if grid
          (text! input-board (clojure.string/join "\n"
                                                  (map #(apply str %) grid)))
          (failure-message "There was an error interpreting the image.")))
      (failure-message "The grid size must be a number."))))

(def interpret-image-button (button :text "Interpret Image"
                                    :listen [:action (fn [x] (read-the-image))]))

(defn browse-file
  "The callback function for the ... button"
  []
  (choose-file :filters [["Images" ["png" "jpeg"]]]
               :success-fn (fn [fc file] (text! in-file (.getAbsolutePath file)))
               ;:remember-directory? true
               :dir "."
               ))

(def browse-button (button :text "..."
                           :listen [:action (fn [x] (browse-file))]))

(defn get-board-array
  "Fetches the text from the input-board text field and applies various filters on it"
  []
  (->> (text input-board)
    string/split-lines
    (map string/lower-case)
    (map string/trim)
    (filter #(not (= % "")))
    (map #(string/replace % " " "*"))
    (map vec)
    vec))

(defn go
  []
  (let [board-array (maybe (check-grid (get-board-array)))]
    (if (not board-array)
      (failure-message "The typed-out grid appears to be invalid.")
      (let [answer (maybe (time-val (solve-flow-graphic board-array)) false)]
        (cond
          (= answer nil) (failure-message "The puzzle appears unsolvable (without bending).")
          (= answer false) (failure-message "There was an error solving the puzzle.\n
Please make sure it's correctly entered.")
          :else (success-message (str "Solved in " (double (/ answer 1000)) " seconds.")))))))

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
   ["What's the puzzle board size?"]
   [resolution-num "wrap"]
   ["Does your version of Flow have ads?"]
   [ads? "wrap"]
   [interpret-image-button "wrap"]
   [go-button "span, align center"]])

(defn -main
  [& args]
  (let [m (mig-panel :items mig-content)
        f (case *compile-gui*
            false (frame :content m)
            true (frame :content m :on-close :exit))]
    (show! (pack! f))))

(if-not *compile-gui*
  (-main))
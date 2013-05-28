(ns flow.timing
  (:use flow.seesaw
        flow.graphics
        flow.reader))

(defn batch-do
  [folder x y]
  (doseq
    [f (file-seq (clojure.java.io/file folder))
     :when (not (.isDirectory f))]
    (let [n (.getAbsolutePath f)
          _ (println n)
          a (time (solve-flow-graphic (file->grid n x)))]
      (if (= a nil)
        (println false)
        (println true))
      )))

(defn average-time
  [folder puzzle-size]
  (let [s (for [f (file-seq (clojure.java.io/file folder))
                :when (not (.isDirectory f))]
            (let [n (.getAbsolutePath f)
                  a (flow.seesaw/time-val (solve-flow-graphic (file->grid n puzzle-size)))
                  ;_ (println a " milliseconds")
                  ]
              a))]
    (/ (apply + s) (count s))))

;(time (solve-flow-graphic (file->grid "/home/alex/temp/sample14x14.JPG" 14 14)))

(defn get-stats
  []
  (let [path "/home/alex/temp/Flowpuzzles/"]
    (doseq [n (range 5 12)]
      (println "Computing average time for" (str n "x" n))
      (println "Average time for" (str n "x" n) ":"
               (double (/ (average-time (str path n "x" n "/") n)
                          1000))
               "seconds"))))

;(get-stats)

;(time (solve-flow-graphic (file->grid "/home/alex/temp/Flowpuzzles/11x11/IMG_1391.PNG" 11 11)))
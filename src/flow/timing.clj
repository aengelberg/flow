(ns flow.timing
  (:use flow.seesaw
        flow.graphics
        flow.reader)
  (:require [clojure.java.io :as io]))

(defn round-to-digits
  [n round-to]
  (let [ten-to-the (int (Math/pow 10 round-to))]
    (double (/ (Math/floor (* n ten-to-the)) ten-to-the))))

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

(defn time-data
  [folder puzzle-size]
  (for [f (file-seq (clojure.java.io/file folder))
        :when (not (.isDirectory f))]
    (let [n (.getAbsolutePath f)
          a (-> (file->grid n puzzle-size)
              solve-flow-graphic
              time-val
              (/ 1000)
              double)
          ;_ (println a " milliseconds")
          ]
      a)))

(defn get-stats
  [& {from :from to :to :or {from 5 to 13}}]
  (let [path "/home/alex/temp/Flowpuzzles/"]
    (doseq [n (range from (inc to))]
      (let [s (map #(round-to-digits % 2)
                   (time-data (str path n "x" n "/") n))]
        (println "         Computing times for" (str n "x" n))
        (print "Data set: ")
        (doseq [x s]
          (print x ""))
        (println)
        (println "Minimum time:"
                 (apply min s)
                 "seconds")
        (println "Maximum time:"
                 (apply max s)
                 "seconds")
        (println "Average time:"
                 (round-to-digits (/ (apply + s) (count s)) 2)
                 "seconds")))))

(defn log-stats
  [filename & args]
  (with-open [output-port (io/writer (io/file filename))]
    (binding [*out* output-port]
      (apply get-stats args))))
(ns flow.core
  (:use flow.reader
        flow.graphics))

(defn batch-do
  [folder x y]
  (doseq
    [f (file-seq (clojure.java.io/file folder))
     :when (not (.isDirectory f))]
    (let [n (.getAbsolutePath f)
          _ (println n)
          a (time (solve-flow-graphic (file->grid n x y)))]
      (if (= a nil)
        (println false)
        (println true))
      )))

;(time (solve-flow-graphic (file->grid "/home/alex/temp/sample14x14.JPG" 14 14)))

;(batch-do "/home/alex/temp/Flowpuzzles/11x11/" 11 11)

(time (solve-flow-graphic (file->grid "/home/alex/temp/Flowpuzzles/11x11/IMG_1391.PNG" 11 11)))
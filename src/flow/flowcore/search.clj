(ns flow.flowcore.search
  (:use flow.flowcore.base)
  (:import java.util.concurrent.PriorityBlockingQueue
           java.util.concurrent.TimeUnit))

(defn make-queue
  [breadth?]
  (cond
    (= breadth? :breadth) clojure.lang.PersistentQueue/EMPTY
    (= breadth? :depth) []))

(defn astar-search-helper
  [queue neighbor-fn finish?-fn update-fn prom]
  (loop []
    (let [peeked (.poll queue 1000 TimeUnit/MILLISECONDS)
          _ (if peeked
              ((or update-fn identity) peeked))]
      (cond
        (realized? prom) nil
        (not peeked) (deliver prom nil)
        (finish?-fn peeked) (deliver prom [peeked])
        :else (do (doseq [neigh (neighbor-fn peeked)]
                    (.offer queue neigh))
                (recur))))))

(defn astar-search-updating
  [start-vals neighbor-fn finish?-fn update-fn]
  (let [queue (PriorityBlockingQueue.)
        prom (promise)]
    (doseq [thing start-vals]
      (.offer queue thing))
    (dotimes [i 3]
      (on-thread #(astar-search-helper queue neighbor-fn finish?-fn update-fn prom)))
    @prom))
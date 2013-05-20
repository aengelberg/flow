(ns flow.flowcore.search
  (:use flow.flowcore.base)
  (:import java.util.concurrent.PriorityBlockingQueue
           java.util.concurrent.TimeUnit))

(defn astar-search-helper
  [queue neighbor-fn finish?-fn update-fn prom]
  (loop []
    (let [peeked (.poll queue 1000 TimeUnit/MILLISECONDS)
          _ (if (and peeked
                     (not (realized? prom)))
              ((or update-fn identity) peeked))]
      (cond
        (realized? prom) nil
        (not peeked) (deliver prom nil)
        (finish?-fn peeked) (deliver prom [peeked])
        :else (do (doseq [neigh (neighbor-fn peeked)]
                    (.offer queue neigh))
                (recur))))))

(defn astar-search-updating
  [& {start-vals :start-vals
      neighbors :neighbors
      finished? :finished?
      update-fn :update-fn
      threads :threads}]
  (let [queue (PriorityBlockingQueue.)
        prom (promise)]
    (doseq [thing start-vals]
      (.offer queue thing))
    (dotimes [i threads]
      (on-thread #(astar-search-helper queue neighbors finished? update-fn prom)))
    @prom))
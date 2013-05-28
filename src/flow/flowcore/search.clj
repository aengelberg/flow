(ns flow.flowcore.search
  (:use flow.flowcore.base)
  (:import java.util.concurrent.PriorityBlockingQueue
           java.util.concurrent.TimeUnit))

(defn astar-search-helper
  [queue neighbor-fn finish?-fn update-fn prom bored my-id]
  (loop []
    (if-not (every? identity @bored)
      (let [peeked (.poll queue 50 TimeUnit/MILLISECONDS)
            _ (if (and peeked
                       (not (realized? prom)))
                ((or update-fn identity) peeked))]
        (if (not peeked)
          (do (dosync (ref-set bored (assoc @bored my-id true)))
            (recur))
          (do
            (dosync (ref-set bored (assoc @bored my-id false)))
            (cond
              (finish?-fn peeked) (deliver prom peeked)
              :else (do (doseq [neigh (neighbor-fn peeked)]
                          (.offer queue neigh))
                      (recur)))))))))

(defn astar-search
  [& {start-val :start-val
      neighbors :neighbors
      finished? :finished?
      update-fn :update-fn
      threads :threads}]
  (let [queue (PriorityBlockingQueue.)
        prom (promise)
        threads (or threads 1)
        bored (ref (vec (repeat threads false)))]
    (.offer queue start-val)
    (dotimes [i threads]
      (on-thread #(astar-search-helper queue neighbors finished? update-fn prom bored i)))
    @prom))

(ns flow.flowcore.search
  (:import java.util.PriorityQueue))

(defn astar-search-updating
  [& {start-val :start-val
      neighbors :neighbors
      finished? :finished?
      update-fn :update-fn}]
  (let [queue (PriorityQueue.)]
    (.offer queue start-val)
    (loop []
      (let [peeked (.poll queue)
            _ ((or update-fn identity) peeked)]
        (cond
          (not peeked) nil
          (finished? peeked) peeked
          :else (do (doseq [neigh (neighbors peeked)]
                      (.offer queue neigh))
                  (recur)))))))
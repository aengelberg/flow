(ns flow.flowcore.search
  (:import java.util.PriorityQueue))

(defn astar-search-updating
  [& {start-vals :start-vals
      neighbors :neighbors
      finished? :finished?
      update-fn :update-fn}]
  (let [queue (PriorityQueue.)]
    (doseq [thing start-vals]
      (.offer queue thing))
    (loop []
      (let [peeked (.poll queue)
            _ ((or update-fn identity) peeked)]
        (cond
          (not peeked) nil
          (finished? peeked) [peeked]
          :else (do (doseq [neigh (neighbors peeked)]
                      (.offer queue neigh))
                  (recur)))))))
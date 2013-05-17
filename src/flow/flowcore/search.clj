(ns flow.flowcore.search
  (:import java.util.PriorityQueue))

(defn make-queue
  [breadth?]
  (cond
    (= breadth? :breadth) clojure.lang.PersistentQueue/EMPTY
    (= breadth? :depth) []))

(defn breadth-search-updating
  [queue neighbor-fn finished?-fn update-fn]
    (if (empty? queue)
      nil
      (let [first-item (peek queue)
            _ (update-fn first-item)
            next-queue (into (pop queue)(neighbor-fn first-item))]
        (if (finished?-fn first-item)
          (lazy-seq (cons first-item (breadth-search-updating next-queue neighbor-fn finished?-fn update-fn)))
          (recur next-queue neighbor-fn finished?-fn update-fn)))))

(defn breadth-search-astar-updating
  [start-vals neighbor-fn finished?-fn update-fn]
  (let [queue (PriorityQueue.)]
    (doseq [thing start-vals]
      (.offer queue thing))
    (loop []
      (let [peeked (.poll queue)
            _ ((or update-fn identity) peeked)]
        (cond
          (not peeked) nil
          (finished?-fn peeked) [peeked]
          :else (do (doseq [neigh (neighbor-fn peeked)]
                      (.offer queue neigh))
                  (recur)))))))

(defn breadth-search
  ([start-vals neighbor-fn finished?-fn breadth? update-fn]
    (if (= breadth? :astar)
      (breadth-search-astar-updating start-vals neighbor-fn finished?-fn update-fn)
      (breadth-search-updating (into (make-queue breadth?) start-vals) neighbor-fn finished?-fn update-fn)))
  ([start-vals neighbor-fn finished?-fn breadth?]
    (if (= breadth? :astar)
      (breadth-search-astar-updating start-vals neighbor-fn finished?-fn nil)
      (breadth-search (into (make-queue breadth?) start-vals) neighbor-fn finished?-fn)))
  ([queue neighbor-fn finished?-fn]
    (if (empty? queue)
      nil
      (let [first-item (peek queue)
            next-queue (into (pop queue)(neighbor-fn first-item))]
        (if (finished?-fn first-item)
          (lazy-seq (cons first-item (breadth-search next-queue neighbor-fn finished?-fn)))
          (recur next-queue neighbor-fn finished?-fn))))))
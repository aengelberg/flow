(ns flow.flowcore.core
  (:use flow.samplegrids
        flow.flowcore.base
        flow.flowcore.search
        flow.flowcore.quickfill)
  (:import java.util.PriorityQueue)
  (:import flow.java.UnionFind)
  )

(def ^:dynamic *thoroughness* 2)
; 0 = don't do possible? while counting neighbors
; 1 = filter possible? while counting neighbors
; 2 = filter possible? AND quickfill while counting neighbors
; presumably, a higher thoroughness increases the time but decreases the growth.

(defn check-for-bending
  [board]
  (every? identity
          (for [x1 (range (dec (count board)))
                y1 (range (dec (count board)))]
            (let [x2 (inc x1)
                  y2 (inc y1)]
              (or (= (get-in board [x1 y1]) \*)
                  (not (= (lowcase (get-in board [x1 y1]))
                          (lowcase (get-in board [x1 y2]))
                          (lowcase (get-in board [x2 y1]))
                          (lowcase (get-in board [x2 y2])))))))))

(defn check-uf
  [this]
  (let [board (:board this)
        rows (count board)
        cols (count (nth board 0))
        posns (:posns this)
        uf (UnionFind. (* rows cols))
        root-map (atom {})
        get-val (fn [i j]
                  (+ j (* i rows)))]
    (doseq [i (range rows)
            j (range cols)
            [i2 j2] [[(inc i) j]
                     [i (inc j)]]
            :when (and (= (get-in board [i j]) \*)
                       (= (get-in board [i2 j2] nil) \*))]
      (.connect uf (get-val i j)(get-val i2 j2)))
    (doseq [i (range rows)
            j (range cols)
            :when (let [v (get-val i j)]
                    (and
                      (= (get-in board [i j]) \*)
                      (= v (get (.array uf) v))))]
      (swap! root-map assoc (get-val i j) false))
    (and (every? identity
                 (for [[color [[x1 y1][x2 y2]]] posns]
                   (or (adjacent? [x1 y1][x2 y2])
                       (let [roots1 (for [[i j] (quick-neighbors board [x1 y1])
                                          :when (= (get-in board [i j]) \*)]
                                      (.findRoot uf (get-val i j)))
                             roots2 (for [[i j] (quick-neighbors board [x2 y2])
                                          :when (= (get-in board [i j]) \*)]
                                      (.findRoot uf (get-val i j)))
                             intersect (clojure.set/intersection (set roots1) (set roots2))]
                         (doseq [root intersect]
                           (swap! root-map assoc root true))
                         (not (empty? intersect))))))
         (every? identity (vals @root-map)))))

(defn possible? [this]
  (and
    (check-uf this)
    (check-for-bending (:board this))
    ))
(defn finished? [this]
  ;(print (. this board))
  (= 0 (:manhattan this)))
(defn neighbors [this]
  (cond
    (finished? this) []
    (not (possible? this)) []
    :else (let [quickfilled (quickfill this)]
            (cond
              (and quickfilled (not (= quickfilled true))) [quickfilled]
              (not quickfilled) []
              :else (let [stuff (into {} (for [[color [p1 p2]] (:posns this)
                                               posn [p1 p2]]
                                           (let [n (if (= posn p1) 0 1)
                                                 neighs (quick-neighbors (:board this) posn color)
                                                 gameposns (for [neigh neighs]
                                                             (let [newGame (make-game-posn (:board this) (:posns this))]
                                                               (expandPosn newGame color n neigh)))
                                                 gameposns (case *thoroughness*
                                                             0 gameposns
                                                             1 (filter possible? gameposns)
                                                             2 (filter quickfill (filter possible? gameposns)))]
                                             [[color (if (= posn p1) 0 1)] gameposns])))
                          [[color n] neighs] (apply min-key #(count (nth % 1)) (seq stuff))]
                      (if (= *thoroughness* 0)
                        (filter possible? neighs)
                        neighs))))
    ))

(defn solve-flow
  [board]
  (let [start (make-game-posn board (color-posn-table board))
        answer (first (astar-search-updating [start] #(neighbors %) #(finished? %) nil))]
    (if answer
      (all-lowcase (:board answer)))))

(defn solve-flow-updating
  [board update-fn]
  (let [start (make-game-posn board (color-posn-table board))
        answer (first (astar-search-updating [start] #(neighbors %) #(finished? %) update-fn))]
    (if answer
      (all-lowcase (:board answer)))))




;(time (solve-flow sample14x14))
(ns flow.flowcore.core
  (:use flow.samplegrids
        flow.flowcore.base
        flow.flowcore.search
        flow.flowcore.quickfill
        flow.flowcore.colorsconnected)
  (:import java.util.PriorityQueue)
  )

(defn check-for-bending
  "Checks if none of the paths are 'bending,' i.e. there isn't a two-by-two area consisting only of any one color."
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

(defn possible?
  "Returns false if this board can't lead to a solution. Ideally returns false early on when the solver is on the wrong path."
  [this]
  (and
    (colors-connected? this)
    (check-for-bending (:board this))
    ))
(defn finished?
  "Is this GamePosn the final board position?"
  [this]
  ;(print (. this board))
  (= 0 (:manhattan this)))

(defn neighbors-helper
  [this]
  (let [thorough-levels {;nothing for 0
                         1 (partial filter possible?)
                         2 (partial filter quickfill)}
        stuff (atom (into {} (for [[color [p1 p2]] (:posns this)
                                   posn [p1 p2]]
                               (let [n (if (= posn p1) 0 1)
                                     neighs (quick-neighbors (:board this) posn color)
                                     gameposns (for [neigh neighs]
                                                 (let [newGame (make-game-posn (:board this) (:posns this))]
                                                   (expandPosn newGame color n neigh)))]
                                 [[color (if (= posn p1) 0 1)] gameposns]))))]
    (loop [current-thoroughness 0]
      (cond
        (= current-thoroughness 2) (let [pair (apply min-key #(count (val %)) (seq @stuff))]
                                     (val pair))
        :else (let [count-map (into {} (for [[[color n] neighs] @stuff]
                                         [[color n] (count neighs)]))
                    current-min (or (first (for [[[color n] x] count-map
                                                 :when (= x 0)]
                                             [color n]))
                                    (first (for [[[color n] x] count-map
                                                 :when (= x 1)]
                                             [color n])))] ;I'd prefer 0, but I'll settle for 1
                (cond
                  current-min (get @stuff current-min)
                  :else (do (reset! stuff
                                    (into {} (for [[[color n] neighs] @stuff]
                                               [[color n] ((get thorough-levels (inc current-thoroughness))
                                                            neighs)])))
                          (recur (inc current-thoroughness)))))))))

(defn neighbors
  "One of the most important pieces of the program; takes a GamePosn and expands it into 0 or more items to enqueue."
  [this]
  (cond
    (finished? this) []
    (not (possible? this)) []
    :else (let [quickfilled (quickfill this)]
            (cond
              (and quickfilled (not (= quickfilled true))) [quickfilled]
              (not quickfilled) []
              :else (neighbors-helper this)))
    ))

(defn solve-flow
  "Takes a board and solves it. Optionally takes :threads (default 1) and :update-fn to call on each dequeued item."
  [board & {:as args}]
  (let [update-fn (:update-fn args)
        start (make-game-posn board (color-posn-table board))
        answer (astar-search :start-val start
                             :neighbors #(neighbors %)
                             :finished? #(finished? %)
                             :update-fn update-fn)]
    (if answer
      (all-lowcase (:board answer)))))




;(time (solve-flow sample14x14))
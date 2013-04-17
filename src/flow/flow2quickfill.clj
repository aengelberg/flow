(ns flow.flow2quickfill
  (:use flow.samplegrids)
  (:import java.util.PriorityQueue)
  (:import flow.java.UnionFind)
  )

(defn lowcase
  [c]
  (first (clojure.string/lower-case c)))
(defn upcase
  [c]
  (first (clojure.string/upper-case c)))
(defn lowcase?
  [c]
  (= c (lowcase c)))
(defn upcase?
  [c]
  (= c (upcase c)))
(defn swapcase
  [c]
  (or
    (first (disj (conj #{} (upcase c) (lowcase c)) c))
    c))

(defn make-queue
  [breadth?]
  (cond
    (= breadth? :breadth) clojure.lang.PersistentQueue/EMPTY
    (= breadth? :depth) []))
(defn breadth-search-updating
  [queue neighbor-fn finish?-fn update-fn]
    (if (empty? queue)
      nil
      (let [first-item (peek queue)
            _ (update-fn first-item)
            next-queue (into (pop queue)(neighbor-fn first-item))]
        (if (finish?-fn first-item)
          (lazy-seq (cons first-item (breadth-search-updating next-queue neighbor-fn finish?-fn update-fn)))
          (recur next-queue neighbor-fn finish?-fn update-fn)))))

(defn breadth-search-astar-updating
  [start-vals neighbor-fn finish?-fn update-fn]
  (let [queue (PriorityQueue.)]
    (doseq [thing start-vals]
      (.offer queue thing))
    (loop []
      (let [peeked (.poll queue)
            _ ((or update-fn identity) peeked)]
        (cond
          (not peeked) nil
          (finish?-fn peeked) [peeked]
          :else (do (doseq [neigh (neighbor-fn peeked)]
                      (.offer queue neigh))
                  (recur)))))))

(defn breadth-search
  ([start-vals neighbor-fn finish?-fn breadth? update-fn]
    (if (= breadth? :astar)
      (breadth-search-astar-updating start-vals neighbor-fn finish?-fn update-fn)
      (breadth-search-updating (into (make-queue breadth?) start-vals) neighbor-fn finish?-fn update-fn)))
  ([start-vals neighbor-fn finish?-fn breadth?]
    (if (= breadth? :astar)
      (breadth-search-astar-updating start-vals neighbor-fn finish?-fn nil)
      (breadth-search (into (make-queue breadth?) start-vals) neighbor-fn finish?-fn)))
  ([queue neighbor-fn finish?-fn]
    (if (empty? queue)
      nil
      (let [first-item (peek queue)
            next-queue (into (pop queue)(neighbor-fn first-item))]
        (if (finish?-fn first-item)
          (lazy-seq (cons first-item (breadth-search next-queue neighbor-fn finish?-fn)))
          (recur next-queue neighbor-fn finish?-fn))))))

(defn quick-span
  ([start-vals neighbor-fn finish?-fn]
    (quick-span (into clojure.lang.PersistentQueue/EMPTY start-vals) neighbor-fn finish?-fn #{}))
  ([queue neighbor-fn finish?-fn visited]
    (cond
      (empty? queue) false
      (finish?-fn (peek queue)) true
      :else (recur (into (pop queue)(filter #(not (visited %)) (neighbor-fn (peek queue))))
                   neighbor-fn
                   finish?-fn
                   (conj visited (peek queue))))))

(defn colorPosnTable [board]
  (apply merge-with vector
         (for [i (range (count board))
               j (range (count (first board))) :when (not (= (get-in board [i j]) \*))]
           {(lowcase (get-in board [i j])) [i j]})))

(defn quick-neighbors
  ([board posn color]
    ;(print board)
    ;(print posn)
    ;(print color)
    (let [[x y] posn]
      (filter #(and (get-in board %)
                    (or (= (get-in board %) \*)
                        (= (get-in board %) color))
                    )
              [[x (inc y)]
               [x (dec y)]
               [(inc x) y]
               [(dec x) y]])))
  ([board posn]
    (let [[x y] posn]
      (filter #(get-in board %)
              [[x (inc y)]
               [x (dec y)]
               [(inc x) y]
               [(dec x) y]]))))

(defn abs [n]
  (if (< n 0)
    (* n -1)
    n))

(defn manhattan [posns]
  (apply + (for [[color [[x1 y1][x2 y2]]] posns]
             (+ (abs (- x1 x2))(abs (- y1 y2))))))

(defrecord GamePosn  ; a (mostly) immutable data structure that gets passed around in the breadth-first search
  [board ;a vector board
   posns ;a map of {\color [[0 0][4 4]], ...}
   manhattan ;the manhattan distance
   ;colorPosnTable ;a color posn table
  ]
  Comparable
  (compareTo [this that]
    (compare manhattan (:manhattan that)))
  )

(declare makeGamePosn)
          

(defn fixPosns [this] ;fix the posns map if there are two colliding same-color nodes
  (let [violating-color (first (for [[color [p1 p2]] (:posns this) :when (= p1 p2)]
                                 [color [p1 p2]]))]
    (if violating-color
        (let [[color [p1 p1]] violating-color
              new-posns (dissoc (:posns this) color)
              new-board (assoc-in (:board this) p1 (upcase (get-in (:board this) p1)))]
          (makeGamePosn new-board new-posns))
        this)))

(defn expandPosn [this color n neighbor]
  ;(print this)
  ;(print color)
  ;(print n)
  ;(print neighbor)
  (fixPosns
    (makeGamePosn
      ;(print posns)
      (assoc-in 
        (assoc-in (:board this) (get-in (:posns this) [color n]) (upcase color))
        neighbor color)
      
      (assoc-in (:posns this) [color n] neighbor))))

(defn smart-expand-posn [this posn-from posn-to]
  (let [board (:board this)
        posns (:posns this)
        color (get-in board posn-from)
        possible-posns (posns color)
        n (if (= (first possible-posns) posn-from)
            0
            1)]
    (expandPosn this color n posn-to)))

(defn check-empties
  "Takes a GamePosn, and returns either another GamePosn (that has been 'quickfilled'), true (implying that it doesn't have an easy quickfill), or nil (implying that it's impossible)."
  [gp]
  (let [board (:board gp)
        posns (:posns gp)
        l (for [x (range (count board))
                y (range (count (nth board 0)))
                :when (= (get-in board [x y]) \*)]
            (let [neighs (quick-neighbors board [x y])
                  neighs (filter #(or (= (get-in board %) \*)(lowcase? (get-in board %))) neighs) ;no upcase
                  heads (filter #(and
                                   (not (= (get-in board %) \*))
                                   (lowcase? (get-in board %))) neighs)
                  empties (filter #(= (get-in board %) \*) neighs)
                  freq (frequencies (map #(get-in board %) heads))
                  dupe-color? (first (for [[k v] freq :when (>= v 2)] k))
                  empties-count (count empties)
                  heads-count (count heads)]
              ;(get {[0 0] false, [0 1] false, [0 2] maybe, [0 3] maybe, [0 4] maybe,
              ;      [1 0] false, [1 1] true, [1 2] true, [1 3] true} ;anything else will have two or more empties, and thus will be true
              ;     [(count empties) (count heads)]
              ;     true)))))))
              (cond
                (>= empties-count 2) true
                (= empties-count 1) (cond
                                      (= heads-count 0) false ;just one empty space? No way!
                                      (= heads-count 1) (-> gp
                                                          (smart-expand-posn (first heads) [x y])
                                                          (smart-expand-posn [x y] (first empties)))
                                      (>= heads-count 2) true)
                ;okay, now empties-count = 0
                (= heads-count 1) false
                (= heads-count 2) (if dupe-color?
                                    (-> gp
                                      (smart-expand-posn (first heads) [x y])
                                      (smart-expand-posn (second heads) [x y]))
                                    false)
                (= heads-count 3) (if dupe-color?  ;Note: same-color? is actually a color
                                    (-> gp
                                      (expandPosn dupe-color? 0 [x y])
                                      (expandPosn dupe-color? 1 [x y]))
                                    false)
                (= heads-count 4) (if dupe-color?
                                    true
                                    false))))]
    (loop [l l]
      (cond
        (empty? l) true
        (= (first l) true) (recur (rest l))
        (= (first l) false) false
        :else (first l)))))

(defn check-for-bending
  [board]
  (every? identity
          (for [x1 (range (dec (count board)))
                y1 (range (dec (count board)))]
            (let [x2 (inc x1)
                  y2 (inc y1)]
              (or (= (get-in board [x1 y1]) \*)
                  (not (= (get-in board [x1 y1])
                          (get-in board [x1 y2])
                          (get-in board [x2 y1])
                          (get-in board [x2 y2]))))))))

(defn adjacent?
  "Takes two posns and tests whether they're adjacent"
  [[x1 y1][x2 y2]]
  (or (and (= y1 y2)
           (= (abs (- x1 x2)) 1))
      (and (= x1 x2)
           (= (abs (- y1 y2)) 1))))

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

(defn isPossible [this]
  (and
    (check-uf this)
    ;(check-empties (:board this))   (now covered in neighbors)
    (check-for-bending (:board this))
    ))
(defn isFinish [this]
  ;(print (. this board))
  (= 0 (:manhattan this)))
(defn neighbors [this]
  (let [quickfilled (check-empties this)]
    (if (and quickfilled (isPossible this))
      (if (isFinish this)
        []
        (or ;(. this update)
          (if (= quickfilled true) false [quickfilled]) ;if quickfilled is true, just do the actual neighbors; otherwise, just return [quickfilled]
            (let [stuff (into {} (for [[color [p1 p2]] (:posns this)
                                       posn [p1 p2]]
                                   [[color (if (= posn p1) 0 1)] (quick-neighbors (:board this) posn color)]))
                  [[color n] neighs] (apply min-key #(count (nth % 1)) (seq stuff))]
              (for [neigh neighs]
                (let [newGame (makeGamePosn (:board this) (:posns this))]
                  (expandPosn newGame color n neigh))))))
      )))



(defn makeGamePosn [board posns]
  (GamePosn. board posns (manhattan posns)))

(defn displayreturn [i] (do (print i) i))

(defn all-lowcase
  [grid]
  (vec (map vec (map #(map lowcase %) grid))))

(defn solve-flow
  [board]
  (let [start (makeGamePosn board (colorPosnTable board))
        answer (first (breadth-search [start] #(neighbors %) #(isFinish %) :astar))]
    (if answer
      (all-lowcase (:board answer)))))

(defn solve-flow-updating
  [board update-fn]
  (let [start (makeGamePosn board (colorPosnTable board))
        answer (first (breadth-search [start] #(neighbors %) #(isFinish %) :astar update-fn))]
    (if answer
      (all-lowcase (:board answer)))))




;(time (solve-flow sample14x14))
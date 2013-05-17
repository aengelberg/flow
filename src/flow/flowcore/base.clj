(ns flow.flowcore.base
  "Some base functions that the other namespaces will use.")

(defn on-thread
  [f]
  (.start (Thread. f)))

(defn displayreturn [i] (do (print i) i))

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

(defn abs [n]
  (if (< n 0)
    (* n -1)
    n))

(defn manhattan [posns]
  (apply + (for [[color [[x1 y1][x2 y2]]] posns]
             (+ (abs (- x1 x2))(abs (- y1 y2))))))

(defn color-posn-table [board]
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



(defrecord GamePosn  ; an immutable data structure that gets passed around in the breadth-first search
  [board ;a vector board
   posns ;a map of {\color [[0 0][4 4]], ...}
   manhattan ;the manhattan distance
  ]
  Comparable
  (compareTo [this that]
    (compare manhattan (:manhattan that)))
  )

(defn make-game-posn [board posns]
  (GamePosn. board posns (manhattan posns)))
          

(defn fixPosns [this] ;fix the posns map if there are two colliding same-color nodes
  (let [violating-color (first (for [[color [p1 p2]] (:posns this) :when (= p1 p2)]
                                 [color [p1 p2]]))]
    (if violating-color
        (let [[color [p1 p1]] violating-color
              new-posns (dissoc (:posns this) color)
              new-board (assoc-in (:board this) p1 (upcase (get-in (:board this) p1)))]
          (make-game-posn new-board new-posns))
        this)))

(defn expandPosn [this color n neighbor]
  ;(print this)
  ;(print color)
  ;(print n)
  ;(print neighbor)
  (fixPosns
    (make-game-posn
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

(defn adjacent?
  "Takes two posns and tests whether they're adjacent"
  [[x1 y1][x2 y2]]
  (or (and (= y1 y2)
           (= (abs (- x1 x2)) 1))
      (and (= x1 x2)
           (= (abs (- y1 y2)) 1))))

(defn all-lowcase
  [grid]
  (vec (map vec (map #(map lowcase %) grid))))
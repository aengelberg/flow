(ns flow.flowcore.quickfill
  (:use flow.flowcore.base))

(defn quickfill-search-helper
  [l]
  (loop [l l]
      (cond
        (empty? l) true
        (= (first l) true) (recur (rest l))
        (= (first l) false) false
        :else (first l))))

(defn empties-quickfill
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
    (quickfill-search-helper l)))

(defn double-empties-quickfill
  [gp]
  (let [{board :board posns :posns} gp
        wall? (fn [x y] (or (not (get-in board [x y])) ; no nil
                            (not (lowcase? (get-in board [x y]))))) ; no \x or \*
        l (for [x (range (count board))
                y (range (count board))
                :when (= (get-in board [x y]) \*)
                [x2 y2] [[(inc x) y] [x (inc y)]]
                :when (= (get-in board [x2 y2]) \*)]
            (if (= x x2)
              (if (and (wall? x (dec y))
                       (wall? x (inc y2)))
                (not (or (and (wall? (inc x) y)
                              (wall? (inc x) y2))
                         (and (wall? (dec x) y)
                              (wall? (dec x) y2))))
                true)
              (if (and (wall? (dec x) y)
                       (wall? (inc x2) y))
                (not (or (and (wall? x (inc y))
                              (wall? x2 (inc y)))
                         (and (wall? x (dec y))
                              (wall? x2 (dec y)))))
                true)))]
    (quickfill-search-helper l)))

(defn quickfill
  "Takes a GamePosn, and returns either another GamePosn (that has been 'quickfilled'), true (implying that it doesn't have an easy quickfill), or nil (implying that it's impossible)."
  ([gp optimizers]
    (cond
      (empty? optimizers) true
      :else (let [optimized ((first optimizers) gp)]
              (cond
                (= optimized true) (recur gp (rest optimizers))
                (= optimized false) false
                :else optimized))))
  ([gp]
    (quickfill gp [empties-quickfill
                   double-empties-quickfill
                   ])))
(ns flow.flowcore.colorsconnected
  (:use flow.flowcore.base))

(defn connected-components
  [board]
  (let [all-empty-posns (for [i (range (count board))
                              j (range (count (first board)))
                              :when (= \* (get-in board [i j]))]
                          [i j])]
    (loop [current-comp 0
           comps {}
           not-marked (set all-empty-posns)
           stack [(first all-empty-posns)]] 
      ;(println current-comp comps not-marked stack)
      (cond
        (and (empty? stack) (empty? not-marked)) comps
        (empty? stack) (recur (inc current-comp)
                              comps
                              not-marked
                              [(first not-marked)])
        :else (let [posn (peek stack)
                    color (get-in board posn)]
                (cond
                  (or (not (= color \*))
                      (not (not-marked posn))) (recur current-comp
                                                      comps
                                                      not-marked
                                                      (pop stack))
                  :else (recur current-comp
                               (assoc comps posn current-comp)
                               (disj not-marked posn)
                               (into (pop stack)
                                     (quick-neighbors board posn)))))))))

(defn colors-connected?
  [this]
  ;(println this)
  (let [{board :board posns :posns} this
        components (connected-components board)
        comp->colors (atom (into {}
                                 (map vector
                                      (distinct (vals components))
                                      (repeat #{}))))
        posn->comps (atom {})]
    (doseq [[color [posn1 posn2]] posns
            :when (not (adjacent? posn1 posn2))
            posn [posn1 posn2]]
      (let [comps (set (for [neigh (quick-neighbors board posn)
                             :when (components neigh)]
                         (components neigh)))]
        (swap! posn->comps assoc posn comps)))
    ;(println comp->colors posn->comps)
    (and
      (every? identity (for [[color [p1 p2]] posns
                             :when (not (adjacent? p1 p2))]
                         (let [intersect (clojure.set/intersection
                                           (@posn->comps p1) (@posn->comps p2))]
                           (doseq [comp intersect]
                             (swap! comp->colors assoc comp (conj (or (@comp->colors comp)
                                                                      #{})
                                                                  color)))
                           (not (empty? intersect)))))
      (every? identity (for [[comp colors] @comp->colors]
                         (not (empty? colors)))))))
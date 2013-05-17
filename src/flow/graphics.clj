(ns flow.graphics
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:use flow.flowcore
        flow.samplegrids
        flow.reader))

(def flow-width 500)
(def flow-height 500)

(def the-board
  (atom
    [[\*]]))

(def reversed-color-settings
  (into {}
        (for [[k v] color-settings]
          [v k])))

(defn random-hexcode []
  (apply str
         "#"
         (for [i (range 6)]
           (nth "56789ABCDEF" (rand-int 11)))))
(defn get-hexcode [c]
  (let [m (reversed-color-settings c)]
    (if (not m)
      (random-hexcode)
      (apply str
             "#"
             (for [n [(m :red)(m :green)(m :blue)]]
               (str
                 (nth "0123456789ABCDEF" (quot n 16))
                 (nth "0123456789ABCDEF" (rem n 16))))))))
(def colormap
  (into {}
        (for [c "abcdefghijklmnopqrstuvwxyz"]
          [c (get-hexcode c)])))

(defn pipe
  [cellX1 cellY1 cellX2 cellY2 cellWidth cellHeight]
  (let [y1 (* (+ cellX1 0.5) cellHeight)
        y2 (* (+ cellX2 0.5) cellHeight)
        x1 (* (+ cellY1 0.5) cellWidth)
        x2 (* (+ cellY2 0.5) cellWidth)]
    (line (int x1)(int y1)(int x2)(int y2))))

(defn draw-flow
  [g2d w h]
  (if (= @the-board nil)
    nil
    (let [board @the-board
          rows (count board)
          cols (count (nth board 0))
          cell-width (/ w cols)
          cell-height (/ h rows)
          padding (* cell-width 0.1)
          pipe-width (* cell-width 0.5)]
      (doseq [i (range rows)
              j (range cols)]
        (let [item (get-in board [i j])
              the-color (get colormap (lowcase item) (color :white))
              x (* j cell-width)
              y (* i cell-height)]
          (draw g2d
                (rect x y cell-width cell-height)
                (style :foreground (color :white)
                       :background (color :black)
                       :stroke (stroke :width (* 0.01 cell-width))))
          (if (and (not (= item \*))(< (count (filter #(= (lowcase item)(lowcase (get-in board %)))
                                                    (quick-neighbors board [i j])))
                                       2))
            (draw g2d
                  (ellipse (+ x padding) (+ y padding) (- cell-width (* padding 2)) (- cell-height (* padding 2)))
                  (style :background the-color)))
          ))
      (doseq [i (range rows)
              j (range cols)]
        (let [item (get-in board [i j])
              the-color (get colormap (lowcase item) (color :white))
              neighs [[(inc i) j]
                      [i (inc j)]]]
          (doseq [[i2 j2] neighs]
            (if (and (get-in board [i2 j2])
                     (not (= item \*))
                     (= (lowcase item)(lowcase (get-in board [i2 j2]))))
              (draw g2d (pipe i j i2 j2 cell-width cell-height) (style :foreground the-color
                                                                       :stroke (stroke :width pipe-width :cap :round)))
              )))))))

(def c (canvas :minimum-size [500 :by 500]
               :background "#FFFFFF"
               :paint #(draw-flow %2 flow-width flow-height)))
(def fp (flow-panel))
(def f (frame :content c :minimum-size [(+ flow-width 10) :by (+ flow-height 35)]))
;(add! fp c)
;(reset! the-board (vec (map vec ["rb" "rb"])))
;(repaint! c)

(defn solve-flow-graphic
  [board]
  (reset! the-board board)
  (show! (pack! f))
  (let [updater (fn [board] 
                  (reset! the-board board)
                  (repaint! c))
        board (solve-flow-updating @the-board
                                   #(let [board (:board %)]
                                      (updater board)))]
    (Thread/sleep 200)
    (when board
      (updater board))
    board))
;(time (solve-flow-graphic (file->grid "photo.PNG" 14 14)))
;(time (solve-flow-graphic (-> (for [i (range 8)]
;                                (for [j (range 8)]
;                                  \*))
;                            (#(map vec %))
;                            vec
;                            (assoc-in [1 1] \a)
;                            (assoc-in [6 6] \a))))
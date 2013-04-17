(ns flow.reader
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:use flow.imagelib)
  (:use flow.samplegrids))

;(def img (read-image "sample14x14.png"))

(def specs
  {:iphone5 {:x1 0
             :y1 (/ 216 1136)
             :x2 1
             :y2 (/ 853 1136)
             :grid-width 1
             :grid-height (/ (- 853 216) 1136)}})  ;note: these are in RATIOS, not actual pixel amounts.

(def resolutions
  {[640 1136] :iphone5})

(defn color-of-cell
  [img phone-type cell-rows cell-cols cell-x cell-y]
  (let [{x1 :x1 y1 :y1
         x2 :x2 y2 :y2
         grid-width :grid-width
         grid-height :grid-height} (specs phone-type)
        width (.getWidth img)
        height (.getHeight img)
        cell-width (/ (* grid-width width) cell-cols)
        cell-height (/ (* grid-height height) cell-rows)
        x-corner (+ (* x1 width)
                    (* (- x2 x1) width
                       (/ cell-x cell-cols)))
        y-corner (+ (* y1 height)
                    (* (- y2 y1) height
                       (/ cell-y cell-rows)))
        x-center (+ x-corner (/ cell-width 2))
        y-center (+ y-corner (/ cell-height 2))]
    (extract-rgb (.getRGB img (int x-center)(int y-center)))))

(defn color-too-dark?
  [color]
  (every? identity
          (for [[k v] color :when (not (= k :alpha))]
            (< v 50))))

(defn close-enough?
  [color1 color2]
  (every? identity
          (for [[k v] color1 :when (not (= color1 :alpha))]
            (< -15 (- v (k color2)) 15))))

(defn get-color-table
  [color-table color]
  (first (for [[k v] color-table :when (close-enough? k color)]
           v)))

(defn image->grid
  [img phone-type cell-rows cell-cols]
  (let [grid (atom (vec (map vec (for [i (range cell-rows)]
                                   (for [j (range cell-cols)]
                                     \*)))))
        color-map (atom color-settings)
        current-char (atom 112)]
    (doseq
      [i (range cell-rows)
       j (range cell-cols)]
      (let [color (color-of-cell img phone-type cell-rows cell-cols i j)]
        ;(println color @color-map (get-color-table @color-map color))
        (cond
          (color-too-dark? color) nil
          (get-color-table @color-map color) (swap! grid assoc-in [j i] (get-color-table @color-map color))
          :else (do (swap! color-map assoc color (char @current-char))
                  (swap! current-char inc)
                  (swap! grid assoc-in [j i] (get-color-table @color-map color))))))
    @grid))

(defn get-phone-type
  [img]
  (resolutions [(.getWidth img)
                (.getHeight img)]))

(defn file->grid
  "Takes a filename of an image, a phone type (:iphone, :iphone5, or :ipad), and the width and height of the grid, and returns a solvable flow grid."
  [filename grid-width grid-height]
  (let [img (read-image filename)]
    (image->grid
      img
      (get-phone-type img)
      grid-height
      grid-width)))
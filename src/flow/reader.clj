(ns flow.reader
  (:import javax.imageio.ImageIO)
  (:import java.io.File)
  (:use flow.imagelib)
  (:use flow.samplegrids))

;(def img (read-image "sample14x14.png"))

(def specs
  "Maps iOS types (plus whether or not they have ads turned on) to the y value of the top edge of the grid."
  {[:iphone4 true] (/ 64 480)
   [:iphone4 false] (/ 180 960)
   [:iphone5 true] (/ 216 1136)
   [:ipad true] (/ 95 1024)})  ;note: these are in RATIOS, not actual pixel amounts.

(def ratios
  {(/ 640 1136) :iphone5
   (/ 320 480) :iphone4
   (/ 768 1024) :ipad})

(defn color-of-cell
  [img puzzle-size cell-x cell-y {phone-type :phone-type, ads? :ads?}]
  (let [;{x1 :x1 y1 :y1
        ; x2 :x2 y2 :y2
        ; grid-width :grid-width
        ; grid-height :grid-height} (specs phone-type)
        width (.getWidth img)
        height (.getHeight img)
        y1 (specs [phone-type ads?])
        y2 (+ y1 width)
        cell-size (/ width puzzle-size)
        x-corner (* cell-size cell-x)
        y-corner (+ (* y1 height)
                    (* cell-size cell-y))
        x-center (+ x-corner (/ cell-size 2))
        y-center (+ y-corner (/ cell-size 2))]
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
  [img puzzle-size phone-info]
  (let [grid (atom (vec (map vec (for [i (range puzzle-size)]
                                   (for [j (range puzzle-size)]
                                     \*)))))
        color-map (atom color-settings)
        current-char (atom 112)]  ; extra chars (starting at \p) for unknown colors
    (doseq
      [i (range puzzle-size)
       j (range puzzle-size)]
      (let [color (color-of-cell img puzzle-size i j phone-info)]
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
  (let [ratio (/ (.getWidth img)
                 (.getHeight img))]
    (or (ratios ratio)
        (throw (Exception. (str "ratio  " ratio " not recognized"))))))

(defn file->grid
  "Takes a filename of an image, a phone type (:iphone, :iphone5, or :ipad), and the width and height of the grid, and returns a solvable flow grid.
Optional (but useful): set :ads? to false if the user has ads turned off, i.e. they've bought an expansion pack. (I think this makes a difference
in the position of the grid on the screen.)"
  [filename puzzle-size & {ads? :ads?}]
  (let [img (read-image filename)
        ads? (or ads? true)]
    (image->grid
      img
      puzzle-size
      {:phone-type (get-phone-type img)
       :ads? ads?})))
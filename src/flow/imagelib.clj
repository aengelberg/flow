(ns flow.imagelib
  (:import javax.imageio.ImageIO
           java.io.File))

(defn read-image
  "Takes a filename and returns a BufferedImage."
  [file]
  (ImageIO/read (File. file)))

(defn write-image
  "Takes a BufferedImage and a filename, and write the image to the file."
  [image file & {type :type :or {:type "bmp"}}]
  (ImageIO/write image type (File. file)))

(defn extract-rgb
  [x]
  {:blue (bit-and x 0xFF)
   :green (bit-shift-right (bit-and x 0xFF00) 8)
   :red (bit-shift-right (bit-and x 0xFF0000) 16)
   :alpha (bit-shift-right (bit-and x 0xFF000000) 24)})

(defn pack-rgb
  [{b :blue g :green r :red a :alpha}]
  (unchecked-int
    (bit-or
      b
      (bit-shift-left g 8)
      (bit-shift-left r 16)
      (bit-shift-left a 24))))
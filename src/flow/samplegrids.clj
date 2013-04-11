(ns flow.samplegrids)
(defn make-grid
  [list-of-strings]
  (vec (map vec (for [s list-of-strings]
                  (for [c s]
                    (if (= c \space) \* c))))))

(def sample5x5
  (vec (map vec
            ["r*g*y"
             "**b*o"
             "*****"
             "*g*y*"
             "*rbo*"])))

(def sample6x6
  (vec (map vec
            ["gyc*rb"
             "****o*"
             "**c***"
             "**r***"
             "g*o***"
             "y*b***"])))

(def sample8x8
  (vec (map vec
            ["y*******"
             "******cg"
             "***f****"
             "*g****cf"
             "****r**o"
             "**o****b"
             "**rb***y"
             "********"])))

(def sample9x9
  (vec (map vec
            ["*********"
             "*rcb*****"
             "***co*ob*"
             "*******f*"
             "*g*gr****"
             "yf*****mp"
             "*py******"
             "*m*******"
             "*********"])))

(def sample10x10
  (vec (map vec
            ["*****c****"
             "**********"
             "*ry***b***"
             "**********"
             "*****yg*gc"
             "*********o"
             "**xpm*m**f"
             "******pow*"
             "*b********"
             "******rxwf"])))

(def otherSample10x10
  (vec (map vec
            ["**********"
             "*b***b**pg"
             "***wp****x"
             "*rg*****mf"
             "*ox*******"
             "***w**f***"
             "**********"
             "r*********"
             "cy****ymo*"
             "******c***"])))

(def sample14x14
  (vec (map vec
            ["**********a***"
             "************b*"
             "***********c**"
             "**************"
             "****de*f******"
             "*******g*f****"
             "*******h******"
             "*****d******ca"
             "****i********j"
             "***gk*k**l**m*"
             "****n*******lj"
             "**o***n****mo*"
             "********h***i*"
             "b**e**********"])))
(def otherSample14x14
  (make-grid
    ["              "
     " n   n        "
     "           o  "
     "        m     "
     " i        d   "
     "   ao l       "
     "  f f  a      "
     "d      l   j  "
     "    j  e c    "
     "    b  b k    "
     "  ge         h"
     "    g     c   "
     "            i "
     "       k mh   "]))
(ns flow.presentation.incanter
  (:require [incanter.core :as in]
            [incanter.charts :as charts]))

(def minimal-data-vecs
  [[5 0.03]
   [6 0.05]
   [7 0.08]
   [8 1.12]
   [9 4.79]
   [10 61.77]])

(def minimal-graph
  (charts/xy-plot (map first minimal-data-vecs)
                  (map second minimal-data-vecs) :title "Strategy #1"))

(def medium-data-vecs
  [[5 0.03]
   [6 0.05]
   [7 0.08]
   [8 0.19]
   [9 0.57]
   [10 2.0]
   [11 5.94]
   [12 36.82]])

(def medium-graph
  (charts/xy-plot (map first minimal-data-vecs)
                  (map second minimal-data-vecs) :title "Strategy #2"))

(charts/add-lines medium-graph
                  (map first medium-data-vecs)
                  (map second medium-data-vecs))

(def best-data-vecs
  [[5 0.02]
   [6 0.04]
   [7 0.09]
   [8 0.32]
   [9 0.79]
   [10 2.32]
   [11 5.95]
   [12 15.62]])

(def best-graph
  (charts/xy-plot (map first minimal-data-vecs)
                  (map second minimal-data-vecs) :title "Strategy #3"))
(charts/add-lines best-graph
                  (map first medium-data-vecs)
                  (map second medium-data-vecs))
(charts/add-lines best-graph
                  (map first best-data-vecs)
                  (map second best-data-vecs))

;(charts/add-polygon test-graph medium-data-vecs)
(ns flow.core
  (:use flow.reader
        flow.graphics)
  (:require flow.seesaw))

(binding [flow.seesaw/*compile-gui* false]
  (flow.seesaw/-main))
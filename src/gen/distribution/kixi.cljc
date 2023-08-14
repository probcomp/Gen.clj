(ns gen.distribution.kixi
  (:require [kixi.stats.distribution :as k]))

(defn bernoulli [p]
  (k/draw (k/bernoulli {:p p})))

(ns gen.distribution.simple-statistics
  (:require ["simple-statistics" :as ss]))

(defn bernoulli [p]
  (first (ss/bernoulliDistribution p)))

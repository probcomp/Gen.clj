(ns gen.distribution.apache-commons-math3
  (:require [gen.distribution :as d])
  (:import (org.apache.commons.math3.distribution
            AbstractIntegerDistribution
            AbstractRealDistribution
            BetaDistribution
            BinomialDistribution
            GammaDistribution
            NormalDistribution
            UniformIntegerDistribution
            UniformRealDistribution)))

(extend-type AbstractRealDistribution
  d/LogPDF
  (logpdf [^AbstractRealDistribution obj v]
    (.logDensity obj v))

  d/Sample
  (sample [^AbstractRealDistribution obj]
    (.sample obj)))

(extend-type AbstractIntegerDistribution
  d/LogPDF
  (logpdf [^AbstractIntegerDistribution obj v]
    (.logProbability obj (Math/floor v)))

  d/Sample
  (sample [^AbstractIntegerDistribution obj]
    (.sample obj)))

;; ## Distributions
;;
;; TODO supply RNG.

(def bernoulli
  (d/dist->gen-fn
   (fn
     ([] (BinomialDistribution. 1 0.5))
     ([p] (BinomialDistribution. 1 p)))
   (fn [n]
     (case n
       0 false
       1 true))
   (fn [b]
     (case b
       true 1
       false 0))))

(def beta
  (d/dist->gen-fn
   (fn [alpha beta]
     (BetaDistribution. alpha beta))))

#_
(def categorical
  "TODO!")

(def gamma
  (d/dist->gen-fn
   (fn [shape scale]
     (GammaDistribution. shape scale))))

(def normal
  (d/dist->gen-fn
   (fn [mean sd]
     (NormalDistribution. mean sd))))

(def uniform
  ;; FIXME Docstring.
  (d/dist->gen-fn
   (fn [low high]
     (UniformRealDistribution. low high))))

(def uniform-discrete
  "Sample an integer from the uniform distribution on the set `{low low+1 ...
  high-1 high}`."
  (d/dist->gen-fn
   (fn [low high]
     (UniformIntegerDistribution. low high))))

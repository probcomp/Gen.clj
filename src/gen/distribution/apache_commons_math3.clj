(ns gen.distribution.apache-commons-math3
  (:require [gen.distribution :as d])
  (:import (org.apache.commons.math3.distribution
            AbstractIntegerDistribution
            AbstractRealDistribution
            BetaDistribution
            GammaDistribution)))

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

;; TODO supply RNG.
(def beta
  (d/dist->gen-fn
   (fn [alpha beta]
     (BetaDistribution. alpha beta))))

(def gamma
  (d/dist->gen-fn
   (fn [shape scale]
     (GammaDistribution. shape scale))))

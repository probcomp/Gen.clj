(ns gen.distribution.commons-math
  "Primitive distributions and generative functions built on
  Apache's [commons-math](https://commons.apache.org/proper/commons-math/)
  library."
  (:require [gen.distribution :as d])
  (:import (org.apache.commons.math3.distribution
            AbstractIntegerDistribution
            AbstractRealDistribution
            BetaDistribution
            BinomialDistribution
            EnumeratedIntegerDistribution
            GammaDistribution
            NormalDistribution
            UniformIntegerDistribution
            UniformRealDistribution)
           (org.apache.commons.math3.random
            JDKRandomGenerator RandomGenerator)))

;; ## Commons-math support

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

;; ## Primitive probability distributions

(defn ^:no-doc rng ^RandomGenerator []
  (JDKRandomGenerator.))

(def bernoulli-distribution
  (-> (fn rec
        ([] (rec 0.5))
        ([p] (BinomialDistribution. (rng) 1 p)))
      (d/encoded (fn [b]
                   (case b
                     true 1
                     false 0))
                 (fn [^long n]
                   (case n
                     0 false
                     1 true)))))

(defn beta-distribution
  ([] (beta-distribution 1.0 1.0))
  ([^double alpha ^double beta]
   (BetaDistribution. (rng) alpha beta)))

(defn gamma-distribution [^double shape ^double scale]
  (GammaDistribution. (rng) shape scale))

(defn normal-distribution
  ([] (normal-distribution 0.0 1.0))
  ([^double mean ^double sd]
   (NormalDistribution. (rng) mean sd)))

(defn uniform-distribution
  ([] (uniform-distribution 0.0 1.0))
  ([^double low ^double high]
   (UniformRealDistribution. (rng) low high)))

(defn uniform-discrete-distribution [low high]
  (UniformIntegerDistribution. (rng) low high))

(defn categorical-distribution [probabilities]
  (let [n  (count probabilities)
        ks (int-array (range n))
        vs (double-array probabilities)]
    (EnumeratedIntegerDistribution. (rng) ks vs)))

;; ## Primitive generative functions

(def bernoulli
  (d/->GenerativeFn bernoulli-distribution))

(def beta
  (d/->GenerativeFn beta-distribution))

(def gamma
  (d/->GenerativeFn gamma-distribution))

(def normal
  (d/->GenerativeFn normal-distribution))

(def uniform
  (d/->GenerativeFn uniform-distribution))

(def uniform-discrete
  "Sample an integer from the uniform distribution on the set `{low low+1 ...
  high-1 high}`."
  (d/->GenerativeFn uniform-discrete-distribution))

(def categorical
  "Given a sequence of probabilities probs where `(reduce + probs)` is 1, sample
  an integer `i` from the set #{1 2 ... (count probs)} with probability `(nth
  probs i)`."
  (d/->GenerativeFn categorical-distribution))

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
            TDistribution
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

;; Small wrapper around a kixi T-distribution to allow for location and scale
;; parameters.

(defrecord LocationScaleT [^TDistribution t-dist location scale]
  d/LogPDF
  (logpdf [_ v]
    (- (.logDensity t-dist (/ (- v location) scale))
       (Math/log scale)))

  d/Sample
  (sample [_]
    (+ location (* scale (.sample t-dist)))))

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

(defn binomial-distribution
  ([n ^double p]
   (BinomialDistribution. (rng) n p)))

(defn beta-distribution
  ([] (beta-distribution 1.0 1.0))
  ([^double alpha ^double beta]
   (BetaDistribution. (rng) alpha beta)))

(defn gamma-distribution [^double shape ^double scale]
  (GammaDistribution. (rng) shape scale))

(defn student-t-distribution
  ([^double nu]
   (TDistribution. (rng) nu))
  ([nu location scale]
   (->LocationScaleT
    (student-t-distribution nu) location scale)))

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

(defn- v->categorical [probabilities]
  (let [n  (count probabilities)
        ks (int-array (range n))
        vs (double-array probabilities)]
    (EnumeratedIntegerDistribution. (rng) ks vs)))

(defn- m->categorical [probabilities]
  (let [ks   (keys probabilities)
        vs   (vals probabilities)
        k->i (zipmap ks (range))
        i->k (zipmap (range) ks)]
    (-> (v->categorical vs)
        (d/->Encoded k->i i->k))))

(defn categorical-distribution
  "Given either

  - a sequence of `probabilities` that sum to 1.0
  - a map of object => probability (whose values sum to 1.0)

  returns a distribution that produces samples of an integer in the range $[0,
  n)$ (where `n == (count probabilities)`), or of a map key (for map-shaped
  `probabilities`)."
  [probabilities]
  (if (map? probabilities)
    (m->categorical probabilities)
    (v->categorical probabilities)))

;; ## Primitive generative functions

(def bernoulli
  (d/->GenerativeFn bernoulli-distribution 1))

(def binomial
  (d/->GenerativeFn binomial-distribution 2))

(def beta
  (d/->GenerativeFn beta-distribution 2))

(def gamma
  (d/->GenerativeFn gamma-distribution 2))

(def student-t
  (d/->GenerativeFn student-t-distribution 3))

(def normal
  (d/->GenerativeFn normal-distribution 2))

(def uniform
  (d/->GenerativeFn uniform-distribution 2))

(def uniform-discrete
  "Sample an integer from the uniform distribution on the set `{low low+1 ...
  high-1 high}`."
  (d/->GenerativeFn uniform-discrete-distribution 2))

(def categorical
  "Given a sequence of probabilities probs where `(reduce + probs)` is 1, sample
  an integer `i` from the set #{1 2 ... (count probs)} with probability `(nth
  probs i)`."
  (d/->GenerativeFn categorical-distribution 1))

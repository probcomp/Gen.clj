(ns gen.distribution.java-util
  (:require [gen.distribution :as d]
            [gen.distribution.math.log-likelihood :as ll])
  (:import (java.util SplittableRandom)))

;; ## Distributions

(defrecord Uniform [^SplittableRandom rnd lo hi]
  d/Sample
  (sample [_]
    (if (zero? lo)
      (.nextDouble rnd hi)
      (+ lo (.nextDouble rnd (- hi lo)))))

  d/LogPDF
  (d/logpdf [_ v]
    (ll/uniform lo hi v)))

(defrecord Bernoulli [^SplittableRandom rnd p]
  d/Sample
  (sample [_]
    (< (.nextDouble rnd) p))

  d/LogPDF
  (d/logpdf [_ v]
    (ll/bernoulli p v)))

(defrecord Gaussian [^SplittableRandom rnd mu sigma]
  d/Sample
  (sample [_]
    (.nextGaussian rnd mu sigma))

  d/LogPDF
  (d/logpdf [_ v]
    (ll/gaussian mu sigma v)))

;; ## Primitive probability distributions

(defn ^:no-doc rng []
  (SplittableRandom.))

(defn bernoulli-distribution
  ([] (bernoulli-distribution 0.5))
  ([p] (->Bernoulli (rng) p)))

(defn uniform-distribution
  ([] (uniform-distribution 0.0 1.0))
  ([lo hi] (->Uniform (rng) lo hi)))

(defn normal-distribution
  ([] (normal-distribution 0.0 1.0))
  ([mu sigma]
   (->Gaussian (rng) mu sigma)))

;; ## Primitive generative functions

(def bernoulli
  (d/->GenerativeFn bernoulli-distribution))

(def uniform
  (d/->GenerativeFn uniform-distribution))

(def normal
  (d/->GenerativeFn normal-distribution))

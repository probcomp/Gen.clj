(ns gen.distribution.kixi
  (:require [gen.distribution :as d]
            [gen.distribution.math.log-likelihood :as ll]
            [kixi.stats.distribution :as k])
  (:import (kixi.stats.distribution Bernoulli Cauchy
                                    Exponential Beta
                                    Gamma Normal Uniform)))

;; ## Kixi.stats protocol implementations
;;
;; NOTE: If we want to seed the PRNG here, the right way to do it is to create
;; wrapper types that hold a kixi distribution instance and an RNG. Then,
;; instead of `draw`, we can call `sample-1` with the distribution and RNG.


(extend-type Bernoulli
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/bernoulli (.-p this) v)))

(extend-type Beta
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/beta (.-alpha this)
             (.-beta this)
             v)))

(extend-type Cauchy
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/cauchy (.-location this)
               (.-scale this)
               v)))

(extend-type Exponential
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/exponential (.-rate this) v)))

(extend-type Uniform
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [min (.-a this)
          max (.-b this)]
      (ll/uniform min max v))))

(extend-type Gamma
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/gamma (.-shape this)
              (.-scale this)
              v)))

(extend-type Normal
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/gaussian (.-mu this)
                 (.-sd this)
                 v)))

;; ## Primitive probability distributions

(defn bernoulli-distribution
  ([] (bernoulli-distribution 0.5))
  ([p] (k/bernoulli {:p p})))

(defn beta-distribution
  ([] (beta-distribution 1.0 1.0))
  ([alpha beta]
   (k/beta {:alpha alpha :beta beta})))

(defn cauchy-distribution [location scale]
  (k/cauchy {:location location :scale scale}))

(defn exponential-distribution [rate]
  (k/exponential {:rate rate}))

(defn uniform-distribution
  ([] (uniform-distribution 0.0 1.0))
  ([lo hi]
   (k/uniform {:a lo :b hi})))

(defn normal-distribution
  ([] (normal-distribution 0.0 1.0))
  ([mu sigma]
   (k/normal {:mu mu :sd sigma})))

(defn gamma-distribution [shape scale]
  (k/gamma {:shape shape :scale scale}))

;; ## Primitive generative functions

(def bernoulli
  (d/->GenerativeFn bernoulli-distribution))

(def beta
  (d/->GenerativeFn beta-distribution))

(def cauchy
  (d/->GenerativeFn cauchy-distribution))

(def exponential
  (d/->GenerativeFn exponential-distribution))

(def uniform
  (d/->GenerativeFn uniform-distribution))

(def normal
  (d/->GenerativeFn normal-distribution))

(def gamma
  (d/->GenerativeFn gamma-distribution))

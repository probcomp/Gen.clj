(ns gen.distribution.kixi
  (:require [gen.distribution :as d]
            [gen.distribution.math.log-likelihood :as ll]
            [kixi.stats.distribution :as k])
  #?(:clj
     (:import (kixi.stats.distribution Bernoulli Cauchy
                                       Exponential Beta
                                       Gamma Normal Uniform T))))

;; ## Kixi.stats protocol implementations
;;
;; NOTE: If we want to seed the PRNG here, the right way to do it is to create
;; wrapper types that hold a kixi distribution instance and an RNG. Then,
;; instead of `draw`, we can call `sample-1` with the distribution and RNG.


(extend-type #?(:clj Bernoulli :cljs k/Bernoulli)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/bernoulli (.-p this) v)))

(extend-type #?(:clj Beta :cljs k/Beta)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/beta (.-alpha this)
             (.-beta this)
             v)))

(extend-type #?(:clj Cauchy :cljs k/Cauchy)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/cauchy (.-location this)
               (.-scale this)
               v)))

(extend-type #?(:clj Exponential :cljs k/Exponential)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/exponential (.-rate this) v)))

(extend-type #?(:clj Uniform :cljs k/Uniform)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [min (.-a this)
          max (.-b this)]
      (ll/uniform min max v))))

(extend-type #?(:clj Gamma :cljs k/Gamma)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/gamma (.-shape this)
              (.-scale this)
              v)))

(extend-type #?(:clj Normal :cljs k/Normal)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/gaussian (.-mu this)
                 (.-sd this)
                 v)))

(extend-type #?(:clj T :cljs k/T)
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (ll/student-t (.-dof this) 0 1 v)))

;; Small wrapper around a kixi T-distribution to allow for location and scale
;; parameters.

(defrecord LocationScaleT [^T t-dist location scale]
  d/Sample
  (sample [_]
    (+ location (* scale (k/draw t-dist))))

  d/LogPDF
  (logpdf [_ v]
    (ll/student-t (.-dof t-dist) location scale v)))

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

(defn student-t-distribution
  ([nu]
   (k/t {:v nu}))
  ([nu location scale]
   (->LocationScaleT (student-t-distribution nu)
                     location
                     scale)))

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

(def student-t
  (d/->GenerativeFn student-t-distribution))

(ns gen.distribution
  "Collection of protocols and functions for working with primitive
  distributions."
  (:require [gen.dynamic.choice-map :as cm]
            [gen.generative-function :as gf]
            [gen.dynamic.trace :as trace])
  #?(:clj
     (:import (clojure.lang IFn))))

;; ## Protocols
;;
;; Any distribution that can implement [[logpdf]] and [[sample]] can implement
;; Gen.clj's generative function interface. These protocols are the way in to the

(defprotocol LogPDF
  (logpdf [this v]
    "Returns the log-likelihood of observing the value `v` given the
    distribution `this`."))

(defprotocol Sample
  (sample [this]
    "Returns a single value sampled from the distribution `this`."))

(defn distribution?
  "Returns true if `t` implements [[LogPDF]] and [[Sample]], false otherwise."
  [t]
  (and (satisfies? LogPDF t)
       (satisfies? Sample t)))

;; ## Primitive Generative Functions

;; The [[gen.distribution/GenerativeFn]] type wraps a constructor `ctor` (a
;; function of args `xs` that returns a statistical distribution) into an object
;; that
;;
;; - acts as a function from `ctor`'s arguments to a single sample
;; - implements the generative function interface defined
;;   in [[gen.generative-function]].
;;
;; This type provides support for all primitive distributions.

(defrecord GenerativeFn [ctor arity]
  gf/IGenerativeFunction
  (has-argument-grads [_] (repeat arity false))

  (accepts-output-grad? [_] false)

  (get-params [_] ())

  (simulate [this args]
    (let [dist  (apply ctor args)
          val   (sample dist)
          score (logpdf dist val)]
      (trace/->PrimitiveTrace this args val score)))

  gf/IGenerate
  (generate [gf args]
    {:weight 0.0
     :trace  (gf/simulate gf args)})

  (generate [gf args constraint]
    (assert (cm/choice? constraint))
    (let [dist   (apply ctor args)
          val    (cm/unwrap constraint)
          weight (logpdf dist val)]
      {:weight weight
       :trace  (trace/->PrimitiveTrace gf args val weight)}))

  #?@(:clj
      [IFn
       (invoke [_]
               (sample (ctor)))
       (invoke [_ a]
               (sample (ctor a)))
       (invoke [_ a b]
               (sample (ctor a b)))
       (invoke [_ a b c]
               (sample (ctor a b c)))
       (invoke [_ a b c d]
               (sample (ctor a b c d)))
       (invoke [_ a b c d e]
               (sample (ctor a b c d e)))
       (invoke [_ a b c d e f]
               (sample (ctor a b c d e f)))
       (invoke [_ a b c d e f g]
               (sample (ctor a b c d e f g)))
       (invoke [_ a b c d e f g h]
               (sample (ctor a b c d e f g h)))
       (invoke [_ a b c d e f g h i]
               (sample (ctor a b c d e f g h i)))
       (invoke [_ a b c d e f g h i j]
               (sample (ctor a b c d e f g h i j)))
       (invoke [_ a b c d e f g h i j k]
               (sample (ctor a b c d e f g h i j k)))
       (invoke [_ a b c d e f g h i j k l]
               (sample (ctor a b c d e f g h i j k l)))
       (invoke [_ a b c d e f g h i j k l m]
               (sample (ctor a b c d e f g h i j k l m)))
       (invoke [_ a b c d e f g h i j k l m n]
               (sample (ctor a b c d e f g h i j k l m n)))
       (invoke [_ a b c d e f g h i j k l m n o]
               (sample (ctor a b c d e f g h i j k l m n o)))
       (invoke [_ a b c d e f g h i j k l m n o p]
               (sample (ctor a b c d e f g h i j k l m n o p)))
       (invoke [_ a b c d e f g h i j k l m n o p q]
               (sample (ctor a b c d e f g h i j k l m n o p q)))
       (invoke [_ a b c d e f g h i j k l m n o p q r]
               (sample (ctor a b c d e f g h i j k l m n o p q r)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s]
               (sample (ctor a b c d e f g h i j k l m n o p q r s)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s t]
               (sample (ctor a b c d e f g h i j k l m n o p q r s t)))
       (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
               (sample (apply ctor a b c d e f g h i j k l m n o p q r s t rest)))
       (applyTo [_ xs]
                (sample (apply ctor xs)))]

      :cljs
      [IFn
       (-invoke [_]
                (sample (ctor)))
       (-invoke [_ a]
                (sample (ctor a)))
       (-invoke [_ a b]
                (sample (ctor a b)))
       (-invoke [_ a b c]
                (sample (ctor a b c)))
       (-invoke [_ a b c d]
                (sample (ctor a b c d)))
       (-invoke [_ a b c d e]
                (sample (ctor a b c d e)))
       (-invoke [_ a b c d e f]
                (sample (ctor a b c d e f)))
       (-invoke [_ a b c d e f g]
                (sample (ctor a b c d e f g)))
       (-invoke [_ a b c d e f g h]
                (sample (ctor a b c d e f g h)))
       (-invoke [_ a b c d e f g h i]
                (sample (ctor a b c d e f g h i)))
       (-invoke [_ a b c d e f g h i j]
                (sample (ctor a b c d e f g h i j)))
       (-invoke [_ a b c d e f g h i j k]
                (sample (ctor a b c d e f g h i j k)))
       (-invoke [_ a b c d e f g h i j k l]
                (sample (ctor a b c d e f g h i j k l)))
       (-invoke [_ a b c d e f g h i j k l m]
                (sample (ctor a b c d e f g h i j k l m)))
       (-invoke [_ a b c d e f g h i j k l m n]
                (sample (ctor a b c d e f g h i j k l m n)))
       (-invoke [_ a b c d e f g h i j k l m n o]
                (sample (ctor a b c d e f g h i j k l m n o)))
       (-invoke [_ a b c d e f g h i j k l m n o p]
                (sample (ctor a b c d e f g h i j k l m n o p)))
       (-invoke [_ a b c d e f g h i j k l m n o p q]
                (sample (ctor a b c d e f g h i j k l m n o p q)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r]
                (sample (ctor a b c d e f g h i j k l m n o p q r)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s]
                (sample (ctor a b c d e f g h i j k l m n o p q r s)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
                (sample (ctor a b c d e f g h i j k l m n o p q r s t)))
       (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
                (sample (apply ctor a b c d e f g h i j k l m n o p q r s t rest)))]))

;; ## Combinators
;;
;; The [[Encoded]] type creates a new distribution from a base distribution
;; `dist`. This new distribution transforms values on the way in to `logpdf`
;; using an `encode` function, and decodes sampled values via `decode`.
;;
;; This is useful for building distributions like categorical distributions that
;; might produce and score arbitrary Clojure values, but lean on some existing
;; numeric base implementation.

(defrecord Encoded [dist encode decode]
  LogPDF
  (logpdf [_ v]
    (logpdf dist (encode v)))

  Sample
  (sample [_]
    (decode (sample dist))))

(defn encoded
  "Given a distribution-producing function `ctor`, returns a constructor for a new
  distribution that

  - encodes each value `v` into `(encode v)` before passage to [[logpdf]]
  - decodes each value `v` sampled from the base distribution into `(decode
    v)`"
  [ctor encode decode]
  (comp #(->Encoded % encode decode) ctor))

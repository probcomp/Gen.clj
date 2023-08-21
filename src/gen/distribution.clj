(ns gen.distribution
  (:require [gen.choice-map :as choice-map]
            [gen.generative-function :as gf]
            [gen.dynamic.trace :as trace])
  (:import (clojure.lang IFn)))

(defprotocol LogPDF
  (logpdf [this v]
    "Log-likelihood of observing the value `v` given the distribution `this`."))

(defprotocol Sample
  (sample [_]))

;; TODO: extend the GFI directly to the distributions... we can't really do this
;; because we have to take args, and at that point we already have an instance.
(defrecord Distribution [ctor encode decode]
  IFn
  (invoke [_]
    (decode (sample (ctor))))
  (invoke [_ arg1]
    (decode (sample (ctor arg1))))
  (invoke [_ arg1 arg2]
    (decode (sample (ctor arg1 arg2))))
  (invoke [_ arg1 arg2 arg3]
    (decode (sample (ctor arg1 arg2 arg3))))
  (applyTo [_ arglist]
    (decode (sample (apply ctor arglist))))

  gf/Simulate
  (simulate [this args]
    (let [dist   (apply ctor args)
          sample (sample dist)
          score  (logpdf dist sample)
          val    (decode sample)]
      (trace/->PrimitiveTrace this args val score)))

  gf/Generate
  (generate [gf args]
    {:weight 0.0
     :trace  (gf/simulate gf args)})
  (generate [gf args constraint]
    (assert (choice-map/choice? constraint))
    (let [dist   (apply ctor args)
          val    (choice-map/unwrap constraint)
          weight (logpdf dist (encode val))]
      {:weight weight
       :trace  (trace/->PrimitiveTrace gf args val weight)})))

(defn dist->gen-fn
  ([ctor]
   (->Distribution ctor identity identity))
  ([ctor encode decode]
   (->Distribution ctor encode decode)))

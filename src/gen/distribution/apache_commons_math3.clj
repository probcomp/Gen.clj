(ns gen.distribution.apache-commons-math3
  (:require [gen.choice-map :as choice-map]
            [gen.dynamic.trace :as trace]
            [gen.generative-function :as gf])
  (:import (org.apache.commons.math3.distribution BetaDistribution)
           (org.apache.commons.math3.distribution GammaDistribution)
           (org.apache.commons.math3.distribution AbstractRealDistribution)))

(defprotocol LogPDF
  (logpdf [_ v]))

(defprotocol Sample
  (sample [_]))

(extend-type AbstractRealDistribution
  LogPDF
  (logpdf [^AbstractRealDistribution obj v]
    (.logDensity obj v))

  Sample
  (sample [^AbstractRealDistribution obj]
    (.sample obj)))

(defrecord Distribution [constructor]
  clojure.lang.IFn
  (invoke [_ arg1]
    (let [^AbstractRealDistribution distribution (constructor arg1)]
      (.sample distribution)))
  (invoke [_ arg1 arg2]
    (let [^AbstractRealDistribution distribution (constructor arg1 arg2)]
      (.sample distribution)))
  (applyTo [_ arglist]
    (let [^AbstractRealDistribution distribution (apply constructor arglist)]
      (.sample distribution)))

  gf/Simulate
  (simulate [this args]
    (let [distribution (apply constructor args)
          sample       (sample distribution)
          score        (logpdf distribution sample)]
      (trace/->PrimitiveTrace this args sample score)))

  gf/Generate
  (generate [gf args]
    {:weight 0
     :trace  (gf/simulate gf args)})
  (generate [gf args constraints]
    (assert (choice-map/choice? constraints))
    (let [retval       constraints
          distribution (apply constructor args)
          weight       (logpdf distribution retval)]
      {:weight weight
       :trace  (trace/->PrimitiveTrace gf args retval weight)})))

(def beta
  (Distribution.
   (fn [alpha beta]
     (BetaDistribution. alpha beta))))

(def gamma
  (Distribution.
   (fn [shape scale]
     (GammaDistribution. shape scale))))

(ns gen.inference.importance
  (:require [clojure.math :as math]
            [gen.distribution.fastmath :as dist]
            [gen.generative-function :as gf]))

;; This implementation comes from `fastmath.core`, ported here for cljc
;; purposes.

(defn- logsumexp
  "log(exp(x1)+...+exp(xn))."
  ^double [xs]
  (loop [[^double x & rst] xs
         r 0.0
         alpha ##-Inf]
    (if (<= x alpha)
      (let [nr (+ r (math/exp (- x alpha)))]
        (if-not (seq rst)
          (+ (math/log nr) alpha)
          (recur rst nr alpha)))
      (let [nr (inc (* r (math/exp (- alpha x))))]
        (if-not (seq rst)
          (+ (math/log nr) x)
          (recur rst nr (double x)))))))

(defn resampling [gf args observations n-samples]
  ;; https://github.com/probcomp/Gen.jl/blob/master/src/inference/importance.jl#L77...L95
  (let [result (gf/generate gf args observations)
        model-trace (volatile! (:trace result))
        log-total-weight (volatile! (:weight result))]
    (dotimes [_ (dec n-samples)]
      (let [candidate (gf/generate gf args observations)
            candidate-model-trace (:trace candidate)
            log-weight (:weight candidate)]
        (vswap! log-total-weight #(logsumexp [log-weight %]))
        (when (dist/bernoulli (math/exp (- log-weight @log-total-weight)))
          (vreset! model-trace candidate-model-trace))))
    (let [log-ml-estimate (- @log-total-weight (math/log n-samples))]
      {:trace @model-trace
       :weight log-ml-estimate})))

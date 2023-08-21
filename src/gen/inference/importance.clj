(ns gen.inference.importance
  (:require [clojure.math :as math]
            [fastmath.core :as fastmath]
            [gen.distribution.fastmath :as dist]
            [gen.dynamic]
            [gen.generative-function :as gf]))

(defn resampling
  "TODO take a flip input instead of depending on `dist`?

  https://github.com/probcomp/Gen.jl/blob/master/src/inference/importance.jl#L77...L95
  "
  [gf args observations n-samples]
  (let [result           (gf/generate gf args observations)
        model-trace      (volatile! (:trace result))
        log-total-weight (volatile! (:weight result))]
    (dotimes [_ (dec n-samples)]
      (let [candidate             (gf/generate gf args observations)
            candidate-model-trace (:trace candidate)
            log-weight            (:weight candidate)]
        (vswap! log-total-weight #(fastmath/logsumexp [log-weight %]))
        (when (dist/bernoulli (math/exp (- log-weight @log-total-weight)))
          (vreset! model-trace candidate-model-trace))))
    (let [log-ml-estimate (- @log-total-weight (math/log n-samples))]
      {:trace  @model-trace
       :weight log-ml-estimate})))

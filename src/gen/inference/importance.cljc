(ns gen.inference.importance
  (:require [gen.choicemap :as choicemap]
            [gen.distribution.kixi :as dist]
            [gen.generative-function :as gf]))

;; This implementation comes from `fastmath.core`, ported here for cljc
;; purposes.

(defn ^:nodoc logsumexp
  "log(exp(x1)+...+exp(xn)). This version can handle infinities."
  ([x] x)
  ([x y]
   (if (<= y x)
     (+ x (Math/log (inc (Math/exp (- y x)))))
     (+ y (Math/log (inc (Math/exp (- x y)))))))
  ([x y & more]
   (reduce logsumexp (logsumexp x y) more)))

(defn- proposal-fn
  ([gen-fn args constraints]
   (if (empty? constraints)
     (fn []
       (gf/propose gen-fn args))
     (let [constraints (choicemap/choicemap constraints)]
       (fn []
         (choicemap/merge
          constraints
          (gf/propose gen-fn args)))))))

(defn sampling
  ([gf args {:keys [observations n-samples proposal proposal-args]
             :or   {proposal-args []
                    observations  choicemap/EMPTY}}]
   ;; TODO do this so it returns a lazy-seq of samples.
   )
  ([gf args observations n-samples]
   (sampling gf args {:observations observations
                      :n-samples    n-samples}))
  ([gf args observations proposal proposal-args n-samples]
   (sampling gf args {:observations  observations
                      :n-samples     n-samples
                      :proposal      proposal
                      :proposal-args proposal-args})))

(defn resampling
  "https://github.com/probcomp/Gen.jl/blob/master/src/inference/importance.jl#L77...L95

  Run sampling importance resampling, returning a single trace.

  TODO do we actually want a sequence of traces encountered, so that we can
  visualize the path?"
  ([gf args {:keys [observations n-samples proposal proposal-args]
             :or   {proposal-args []
                    observations  choicemap/EMPTY
                    n-samples     10}}]
   (let [constraint-fn (if proposal
                         (proposal-fn proposal proposal-args observations)
                         (fn [] {:choices observations
                                :weight  0.0}))
         {proposal-choices :choices
          proposal-weight :weight} (constraint-fn)
         result        (gf/generate gf args proposal-choices)]
     (loop [i                (dec n-samples)
            model-trace      (:trace result)
            log-total-weight (- (:weight result) proposal-weight)]
       (if (zero? i)
         (let [log-ml-estimate (- log-total-weight (Math/log n-samples))]
           {:trace  model-trace
            :weight log-ml-estimate})
         (let [{proposal-choices :choices
                proposal-weight :weight} (constraint-fn)
               candidate       (gf/generate gf args proposal-choices)
               candidate-trace (:trace candidate)
               log-weight      (- (:weight candidate) proposal-weight)
               new-weight      (logsumexp log-total-weight log-weight)
               new-trace       (if (dist/bernoulli (Math/exp (- log-weight new-weight)))
                                 candidate-trace
                                 model-trace)]
           (recur (unchecked-dec-int i) new-trace new-weight))))))
  ([gf args observations n-samples]
   (resampling gf args {:observations observations
                        :n-samples    n-samples}))
  ([gf args observations proposal proposal-args n-samples]
   (resampling gf args {:observations  observations
                        :n-samples     n-samples
                        :proposal      proposal
                        :proposal-args proposal-args})))

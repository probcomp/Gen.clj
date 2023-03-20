(ns gen.distribution.fastmath
  (:require [fastmath.random :as random]
            [gen]
            [gen.choice-map :as choice-map]
            [gen.diff :as diff]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

;; https://generateme.github.io/fastmath/fastmath.random.html#var-distribution
;; https://www.gen.dev/docs/stable/ref/distributions/#Probability-Distributions-1

(defrecord Trace [gf args retval score]
  trace/Args
  (args [_] args)

  trace/Choices
  (choices [_]
    (dynamic.choice-map/choice retval))

  trace/GenFn
  (gf [_] gf)

  trace/RetVal
  (retval [_] retval)

  trace/Score
  (score [_]
    score)

  trace/Update
  (update [prev-trace constraints]
    (-> (cond (dynamic.choice-map/choice? constraints)
              (-> (gf/generate gf (trace/args prev-trace) constraints)
                  (update :weight - (trace/score prev-trace))
                  (assoc :discard (dynamic.choice-map/choice (trace/retval prev-trace))))

              (dynamic.choice-map/choice-map? constraints)
              (throw (ex-info "Expected a value at address but found a sub-assignment."
                              {:sub-assignment constraints}))

              :else
              {:trace prev-trace
               :weight 0}) ; (- (logpdf (trace/retval prev-trace)) (trace/score prev-trace))
        (assoc :change diff/unknown-change))))

(defn trace
  "Creates a new fastmath distribution trace."
  [gf args retval score]
  (->Trace gf args retval score))

(defn fastmath-distribution
  "Creates a new fastmath distribution."
  ([k args->config]
   (fastmath-distribution k args->config identity identity))
  ([k args->config fastmath-sample->sample sample->fastmath-sample]
   (let [args->dist (fn [& args]
                      (random/distribution k (apply args->config args)))
         args->fastmath-sample (fn [& args]
                                 (random/sample (apply args->dist args)))]
     (with-meta (comp fastmath-sample->sample args->fastmath-sample)
       {`gf/simulate (fn [gf args]
                       (let [config (apply args->config args)
                             fastmath-dist (random/distribution k config)
                             fastmath-sample (apply args->fastmath-sample args)
                             retval (fastmath-sample->sample fastmath-sample)
                             score (random/log-likelihood fastmath-dist [fastmath-sample])]
                         (trace gf args retval score)))
        `gf/generate (fn [gf args constraints]
                       (if (dynamic.choice-map/choice? constraints)
                         (let [retval (choice-map/value constraints)
                               config (apply args->config args)
                               fastmath-dist (random/distribution k config)
                               fastmath-sample (sample->fastmath-sample retval)
                               weight (random/log-likelihood fastmath-dist [fastmath-sample])
                               trace (trace gf args retval weight)]
                           {:weight weight
                            :trace trace})
                         (let [trace (gf/simulate gf args)]
                           {:weight 0
                            :trace trace})))}))))

(def bernoulli
  "Samples a Bool value which is true with given probability."
  (fastmath-distribution
   :bernoulli
   (fn
     ([] {:p 0.5})
     ([p] {:p p}))
   (fn [n]
     (case n
       0 false
       1 true))
   (fn [b]
     (case b
       true 1
       false 0))))

(def beta
  (fastmath-distribution
   :beta
   (fn [alpha beta]
     {:alpha alpha
      :beta beta})))

(def categorical
  "Given a sequence of probabilities probs where `(reduce + probs)` is 1, sample
  an integer `i` from the set #{1 2 ... (count probs)} with probability `(nth probs
  i)`."
  (fastmath-distribution
   :categorical-distribution
   (fn [probs]
     {:data (range (count probs))
      :probabilities probs})))

(def gamma
  (fastmath-distribution
   :gamma
   (fn [shape scale]
     {:shape shape
      :scale scale})))

(def normal
  (fastmath-distribution
   :normal
   (fn [mu std]
     {:mu mu
      :sd std})))

(def uniform
  ;; FIXME Docstring.
  (fastmath-distribution
   :uniform-real
   (fn [low high]
     {:lower low
      :upper high})))

(def uniform-discrete
  "Sample an integer from the uniform distribution on the set `{low low+1 ...
  high-1 high}`."
  (fastmath-distribution
   :uniform-int
   (fn [low high]
     {:lower low
      :upper high})))

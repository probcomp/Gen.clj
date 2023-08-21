(ns gen.distribution.fastmath
  (:require [fastmath.random :as random]
            [gen.distribution :as d]))

(defn fastmath-distribution
  "Creates a new fastmath distribution."
  ([k args->config]
   (fastmath-distribution k args->config identity identity))
  ([k args->config fastmath-sample->sample sample->fastmath-sample]
   (d/dist->gen-fn
    :ctor (fn [& args]
            (random/distribution k (apply args->config args)))
    :sample-fn random/sample
    :score-fn (fn [dist sample]
                (random/log-likelihood dist [sample]))
    :encode sample->fastmath-sample
    :decode fastmath-sample->sample)))

(def bernoulli
  "Samples a Bool value which is true with given probability."
  (fastmath-distribution
   :bernoulli
   (fn
     ([] {:p 0.5})
     ([p] {:p p}))
   (fn [^long n]
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

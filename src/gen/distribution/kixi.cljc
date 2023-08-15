(ns gen.distribution.kixi
  (:require [gen.choice-map :as choice-map]
            [gen.diff :as diff]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace]
            [kixi.stats.distribution :as k]))

(defprotocol IScore
  (score [_ v]
    "Log-likelihood of observing the value `v` given the distribution `this`."))

(def log-2pi (Math/log (* 2 Math/PI)))

(def gamma-coefficients
  [76.18009172947146
   -86.50532032941677
   24.01409824083091
   -1.231739572450155
   0.1208650973866179e-2
   -0.5395239384953e-5])

(defn log-gamma [xx]
  (let [x       (dec xx)
        tmp     (+ x 5.5)
        tmp     (- tmp (* (+ x 0.5) (Math/log tmp)))
        [_ ser] (reduce (fn [[x ser] coeff]
                          (let [x+1 (inc x)]
                            [x+1 (+ ser (/ coeff x+1))]))
                        [x 1.000000000190015]
                        gamma-coefficients)]
    (+ (- tmp) (Math/log (* 2.5066282746310005 ser)))))

(extend-protocol IScore
  kixi.stats.distribution.Bernoulli
  (score [this v]
    (let [p (.-p this)]
      (Math/log
       (if v p (- 1.0 p)))))

  kixi.stats.distribution.Uniform
  (score [this v]
    (let [a (.-a this)
          b (.-b this)]
      (if (or (< v a) (> v b))
        (- ##Inf)
        (- (Math/log (- b a))))))

  kixi.stats.distribution.Gamma
  (score [this v]
    (let [shape (.-shape this)
          scale (.-scale this)]
      (- (* (dec shape) (Math/log v))
         (/ v scale)
         (log-gamma shape)
         (* shape (Math/log scale)))))

  kixi.stats.distribution.Normal
  (score [this v]
    (let [mu       (.-mu this)
          v-mu     (- v mu)
          v-mu-sq  (* v-mu v-mu)
          sigma    (.-sd this)
          variance (* sigma sigma)]
      (* -0.5 (+ log-2pi
                 (* 2 (Math/log sigma))
                 (/ v-mu-sq variance))))))

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
    ;; TODO what is different here between this and the commons math one?
    ;;
    ;; NOTE I think it is only the `gf` argument, and that itself is handled by
    ;; protocols. So we can probably abstract this out.
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
  "Creates a new kixi distribution trace."
  [gf args retval score]
  (->Trace gf args retval score))

(defn kixi-distribution
  ([f args->config]
   (kixi-distribution f args->config identity identity))
  ([f args->config kixi-sample->sample sample->kixi-sample]
   (let [args->dist        (fn [& args]
                             (f (apply args->config args)))
         args->kixi-sample (fn [& args]
                             (k/draw (apply args->dist args)))]
     (with-meta (comp kixi-sample->sample args->kixi-sample)
       {`gf/simulate
        (fn [gf args]
          (let [config      (apply args->config args)
                kixi-dist   (f config)
                kixi-sample (apply args->kixi-sample args)
                retval      (kixi-sample->sample kixi-sample)
                score       (score kixi-dist kixi-sample)]
            (trace gf args retval score)))
        `gf/generate
        (fn
          ([gf args]
           {:weight 0
            :trace  (gf/simulate gf args)})
          ([gf args constraints]
           (assert (dynamic.choice-map/choice? constraints))
           (let [retval      (choice-map/value constraints)
                 config      (apply args->config args)
                 kixi-dist   (f config)
                 kixi-sample (sample->kixi-sample retval)
                 weight      (score kixi-dist kixi-sample)
                 trace       (trace gf args retval weight)]
             {:weight weight
              :trace  trace})))}))))

(defn primitive [f]
  (fn [& xs]
    (let [dist (apply f xs)]
      (k/draw dist))))

;; fastmath.random> (lpdf (distribution :binomial {:trials 12 :p 0.25}) 6)
;; -3.2151465297883446

;; fastmath.random> (lpdf (distribution :bernoulli {:p 0.25}) 1)
;; -1.3862943611198906

(def bernoulli
  (kixi-distribution
   k/bernoulli
   (fn
     ([] {:p 0.5})
     ([p] {:p p}))))

(def normal
  (kixi-distribution
   k/normal
   (fn [mu std]
     {:mu mu
      :sd std})))

(def uniform
  (kixi-distribution
   k/uniform
   (fn [low high]
     {:a low
      :b high})))

(def gamma
  (kixi-distribution
   k/gamma
   (fn [shape scale]
     {:shape shape
      :scale scale})))

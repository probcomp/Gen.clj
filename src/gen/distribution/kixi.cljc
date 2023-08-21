(ns gen.distribution.kixi
  (:require [gen.distribution :as d]
            [kixi.stats.distribution :as k]))

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

(extend-type kixi.stats.distribution.Bernoulli
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [p (.-p this)]
      (Math/log
       (if v p (- 1.0 p))))))

(extend-type kixi.stats.distribution.Uniform
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [a (.-a this)
          b (.-b this)]
      (if (or (< v a) (> v b))
        (- ##Inf)
        (- (Math/log (- b a)))))))

(extend-type kixi.stats.distribution.Gamma
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [shape (.-shape this)
          scale (.-scale this)]
      (- (* (dec shape) (Math/log v))
         (/ v scale)
         (log-gamma shape)
         (* shape (Math/log scale))))))

(extend-type kixi.stats.distribution.Normal
  d/Sample
  (sample [this] (k/draw this))

  d/LogPDF
  (logpdf [this v]
    (let [mu       (.-mu this)
          v-mu     (- v mu)
          v-mu-sq  (* v-mu v-mu)
          sigma    (.-sd this)
          variance (* sigma sigma)]
      (* -0.5 (+ log-2pi
                 (* 2 (Math/log sigma))
                 (/ v-mu-sq variance))))))

(defn kixi-distribution
  ([f]
   (d/dist->gen-fn f))
  ([f kixi-sample->sample sample->kixi-sample]
   (d/dist->gen-fn
    f
    sample->kixi-sample
    kixi-sample->sample)))

(def bernoulli
  (kixi-distribution
   (fn
     ([] (k/bernoulli {:p 0.5}))
     ([p] (k/bernoulli {:p p})))))

(def normal
  (kixi-distribution
   (fn [mu std]
     (k/normal {:mu mu :sd std}))))

(def uniform
  (kixi-distribution
   (fn [low high]
     (k/uniform {:a low :b high}))))

(def gamma
  (kixi-distribution
   (fn [shape scale]
     (k/gamma {:shape shape :scale scale}))))

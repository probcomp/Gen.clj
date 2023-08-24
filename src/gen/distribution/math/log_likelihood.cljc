(ns gen.distribution.math.log-likelihood
  "Log-likelihood implementations for various primitive distributions.")

;; ## Helpful constants
;;
;; These come in handy in the implementations below and are worth caching.

(def ^:no-doc log-pi
  (Math/log Math/PI))

(def ^:no-doc log-2pi
  (Math/log (* 2 Math/PI)))

(def ^:no-doc sqrt-2pi
  (Math/sqrt (* 2 Math/PI)))

;; ## Log-likelihood implementations

(def ^:no-doc gamma-coefficients
  "Coefficients for the Lanczos approximation to the natural log of the Gamma
  function described in [section 6.1 of Numerical
  Recipes](http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-1.pdf)."
  [76.18009172947146
   -86.50532032941677
   24.01409824083091
   -1.231739572450155
   0.1208650973866179e-2
   -0.5395239384953e-5])

(defn ^:no-doc log-gamma-fn
  "Returns the natural log of the value of the [Gamma
  function](https://en.wikipedia.org/wiki/Gamma_function) evaluated at `x`

  This function implements the Lanczos approximation described in [section 6.1
  of Numerical Recipes](http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-1.pdf)."
  [x]
  (let [tmp (+ x 5.5)
        tmp (- (* (+ x 0.5) (Math/log tmp)) tmp)
        n   (dec (count gamma-coefficients))
        ser (loop [i   0
                   x+1 (inc x)
                   acc 1.000000000190015]
              (if (> i n)
                acc
                (let [coef (nth gamma-coefficients i nil)]
                  (recur (inc i)
                         (inc x+1)
                         (+ acc (/ coef x+1))))))]
    (+ tmp (Math/log (* sqrt-2pi (/ ser x))))))

(defn gamma
  "Returns the log-likelihood of the [Gamma
  distribution](https://en.wikipedia.org//wiki/Gamma_distribution) parameterized
  by `shape` and `scale` at the value `v`.

  The implementation follows the algorithm described on the Gamma
  distribution's [Wikipedia
  page](https://en.wikipedia.org//wiki/Gamma_distribution#Maximum_likelihood_estimation)."
  [shape scale v]
  (if (pos? v)
    (- (* (dec shape) (Math/log v))
       (/ v scale)
       (log-gamma-fn shape)
       (* shape (Math/log scale)))
    ##-Inf))

(defn log-beta-fn
  "Returns the natural log of the value of the [Beta
  function](https://en.wikipedia.org/wiki/Beta_function) evaluated at inputs `a`
  and `b`."
  [a b]
  (- (+ (log-gamma-fn a)
        (log-gamma-fn b))
     (log-gamma-fn (+ a b))))

(defn beta
  "Returns the log-likelihood of the [Beta
  distribution](https://en.wikipedia.org/wiki/Beta_distribution) parameterized by
  `alpha` and `beta` at the value `v`.

  The implementation follows the algorithm described on the Beta
  distribution's [Wikipedia
  page](https://en.wikipedia.org/wiki/Beta_distribution#Probability_density_function)."
  [alpha beta v]
  {:pre [(pos? alpha) (pos? beta)]}
  (if (< 0 v 1)
    (- (+ (* (- alpha 1) (Math/log v))
          (* (- beta alpha) (Math/log (- 1 v))))
       (log-beta-fn alpha beta))
    ##-Inf))

(defn bernoulli
  "Returns the log-likelihood of a [Bernoulli
  distribution](https://en.wikipedia.org/wiki/Bernoulli_distribution)
  parameterized by probability `p` at the boolean value `v`."
  [p v]
  {:pre [(<= 0 p 1)]}
  (Math/log (if v p (- 1.0 p))))

(defn cauchy
  "Returns the log-likelihood of a [Cauchy
  distribution](https://en.wikipedia.org/wiki/Cauchy_distribution) parameterized
  by `scale` and `location` at the value `v`.

  The implementation follows the algorithm described on the Cauchy
  distribution's [Wikipedia
  page](https://en.wikipedia.org/wiki/Cauchy_distribution#Probability_density_function_(PDF))."
  [scale location v]
  (let [normalized (/ (- v location) scale)
        norm**2    (* normalized normalized)]
    (- (- log-pi)
       (Math/log scale)
       (Math/log (+ 1 norm**2)))))

(defn delta
  "Returns the log-likelihood of the [Dirac delta
  distribution](https://en.wikipedia.org/wiki/Dirac_delta_function) centered
  around `center` at the value `v`."
  [center v]
  (if (= center v) 0.0 ##-Inf))

(defn exponential
  "Returns the log-likelihood of the [exponential
  distribution](https://en.wikipedia.org/wiki/Exponential_distribution) with
  rate parameter `rate` at the value `v`."
  [rate v]
  (if (>= v 0)
    (- (Math/log rate) (* rate v))
    ##-Inf))

(defn laplace
  "Returns the log-likelihood of the [Laplace
  distribution](https://en.wikipedia.org/wiki/Laplace_distribution) with
  `location` and `scale` parameters at the value `v`.

  The implementation follows the algorithm described on the Laplace
  distribution's [Wikipedia
  page](https://en.wikipedia.org/wiki/Laplace_distribution#Probability_density_function)."
  [location scale v]
  (- (+ (Math/log (* 2.0 scale))
        (/ (Math/abs ^double (- v location))
           scale))))

(defn gaussian
  "Returns the log-likelihood of the [Gaussian
  distribution](https://en.wikipedia.org/wiki/Gaussian_distribution) with
  mean `mu` and standard deviation `sigma` at the value `v`.

  The implementation follows the algorithm described on the Gaussian
  distribution's [Wikipedia
  page](https://en.wikipedia.org/wiki/Normal_distribution#Operations_on_a_single_normal_variable):

  Given $z = \\left(\\frac{x-\\mu}{\\sigma}\\right)^2$:

  $$
  \\begin{aligned}
  \\ln p(x) &= -\\frac{1}{2}z^2 - \\ln (\\sigma \\sqrt{2\\pi}) \\\\
            &= -\\frac{1}{2}\\left(z^2 + 2 \\ln (\\sigma \\sqrt{2\\pi}) \\right) \\\\
            &= -\\frac{1}{2}\\left(z^2 + \\ln (\\sigma^2) + \\ln (2\\pi) \\right)
  \\end{aligned}
  $$"
  [mu sigma v]
  (let [v-mu     (- v mu)
        v-mu-sq  (* v-mu v-mu)
        variance (* sigma sigma)]
    (* -0.5 (+ log-2pi
               (Math/log variance)
               (/ v-mu-sq variance)))))


(defn uniform
  "Returns the log-likelihood of the continuous [uniform
  distribution](https://en.wikipedia.org/wiki/Continuous_uniform_distribution)
  with inclusive lower bound `a` and inclusive upper bound `b` at the value
  `v`."
  [a b v]
  (if (<= a v b)
    (- (Math/log (- b a)))
    ##-Inf))

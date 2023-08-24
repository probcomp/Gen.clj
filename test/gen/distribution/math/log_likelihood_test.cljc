(ns gen.distribution.math.log-likelihood-test
  (:require [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [gen.distribution.math.log-likelihood :as ll]
            [same.core :refer [ish? zeroish? with-comparator]]))

(defn within
  "Returns a function that tests whether two values are within `eps` of each
  other."
  [^double eps]
  (fn [^double x ^double y]
    (< (Math/abs (- x y)) eps)))

(defn factorial
  "Factorial implementation for testing."
  [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn gen-double [min max]
  (gen/double*
   {:min min :max max :infinite? false :NaN? false}))

(deftest log-gamma-fn-tests
  (testing "log-Gamma ~matches log(factorial)"
    (with-comparator (within 1e-11)
      (doseq [n (range 1 15)]
        (is (ish? (Math/log (factorial (dec n)))
                  (ll/log-gamma-fn n))))))


  (with-comparator (within 1e-12)
    (checking "Euler's reflection formula"
              [z (gen-double 0.001 0.999)]
              (is (ish? (+ (ll/log-gamma-fn (- 1 z))
                           (ll/log-gamma-fn z))
                        (- ll/log-pi
                           (Math/log
                            (Math/sin (* Math/PI z)))))))))

(deftest gamma-tests
  (testing "spot checks"
    (is (= -6.391804444241573 (ll/gamma 0.001 1 0.4)))
    (is (= -393.0922447210179 (ll/gamma 1 0.001 0.4)))))

(deftest beta-tests
  (checking "(log of the) Beta function is symmetrical"
            [a (gen-double 0.01 2)
             b (gen-double 0.01 2)]
            (is (= (ll/log-beta-fn a b)
                   (ll/log-beta-fn b a))))

  (testing "spot checks"
    (is (= -6.5026956359820804 (ll/beta 0.001 1 0.4)))
    (is (= -6.397440480839912 (ll/beta 1 0.001 0.4)))))

(deftest bernoulli-tests
  (checking "Bernoulli properties"
            [p (gen-double 0 1)
             v gen/boolean]
            (is (= (ll/bernoulli 0.5 v)
                   (ll/bernoulli 0.5 (not v)))
                "Fair coin has equal chance")

            (is (ish? 1.0
                      (+ (Math/exp (ll/bernoulli p v))
                         (Math/exp (ll/bernoulli p (not v)))))
                "All options sum to 1")))

(deftest cauchy-tests
  (checking "Cauchy properties"
            [scale (gen-double 0.001 100)
             v     (gen-double -100 100)]
            (is (= (ll/cauchy scale 0 v)
                   (ll/cauchy scale 0 (- v)))
                "symmetric about location"))

  (testing "spot checks"
    (is (= -1.1447298858494002 (ll/cauchy 1 1 1)))
    (is (= -1.8378770664093453 (ll/cauchy 2 2 2)))))

(deftest delta-tests
  (checking "Delta properties"
            [center (gen-double -100 100)
             v      (gen-double -100 100)]
            (if (= center v)
              (is (= 0.0    (ll/delta center v)))
              (is (= ##-Inf (ll/delta center v))))))

(deftest exponential-tests
  (checking "exponential will never produce negative values"
            [rate (gen-double -100 100)
             v    (gen-double -100 -0.00001)]
            (is (= ##-Inf (ll/exponential rate v))))

  (checking "rate 1.0 produces -v"
            [v (gen-double 0 100)]
            (is (= (- v) (ll/exponential 1.0 v))))

  (checking "rate 0.0 produces #-Inf"
            [v (gen-double -100 100)]
            (is (= ##-Inf (ll/exponential 0.0 v))))

  (testing "spot checks"
    (is (= -3.3068528194400546 (ll/exponential 2.0 2.0)))
    (is (= -5.306852819440055  (ll/exponential 2.0 3.0)))))

(deftest laplace-test
  (checking "Laplace properties"
            [v (gen-double -10 10)]
            (let [log-l (ll/laplace 0 1 v)]
              (is (if (neg? v)
                    (is (= log-l (- v (Math/log 2))))
                    (is (= log-l (- (- v) (Math/log 2)))))
                  "location 0, scale 1"))

            (is (= (ll/laplace 0 1 v)
                   (ll/laplace 0 1 (- v)))
                "symmetric about location"))

  (checking "Laplace with scale 1, location == v"
            [v (gen-double -10 10)]
            (is (is (= (- (Math/log 2))
                       (ll/laplace v 1 v)))))

  (testing "spot checks"
    (is (= -1.6931471805599454 (ll/laplace 2 1 1)))
    (is (= -1.8862943611198906 (ll/laplace 0 2 1)))
    (is (= 4.214608098422191   (ll/laplace 0 0.001 0.002)))))

(deftest gaussian-tests
  (checking "Gaussian properties"
            [mu    (gen-double -10 10)
             sigma (gen-double 0.001 10)
             v     (gen-double -100 100)
             shift (gen-double -10 10)]
            (is (ish? (ll/gaussian 0.0 sigma v)
                      (ll/gaussian 0.0 sigma (- v)))
                "Gaussian is symmetric about the mean")

            (is (ish? (ll/gaussian mu sigma v)
                      (ll/gaussian (+ mu shift) sigma (+ v shift)))
                "shifting by the mean is a symmetry"))


  (testing "spot checks"
    (is (= -1.0439385332046727 (ll/gaussian 0 1 0.5)))
    (is (= -1.643335713764618 (ll/gaussian 0 2 0.5)))
    (is (= -1.612085713764618 (ll/gaussian 0 2 0)))))

(deftest uniform-tests
  (checking "(log of the) Beta function is symmetrical"
            [min (gen-double -10 0)
             max (gen-double 0 10)
             v   (gen-double -10 10)]
            (let [log-l (ll/uniform min max v)]
              (if (<= min v max)
                (is (zeroish?
                     (+ log-l (Math/log (- max min))))
                    "Inside the bounds, log-l*range == 1.0")
                (is (= ##-Inf log-l)
                    "Outside the bounds, (log 0.0)")))))

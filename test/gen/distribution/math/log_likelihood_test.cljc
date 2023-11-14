(ns gen.distribution.math.log-likelihood-test
  (:require [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test :refer [deftest is testing]]
            [gen.distribution.math.log-likelihood :as ll]
            [gen.distribution :as distribution]
            [gen.distribution-test :as dt]
            [gen.generators :refer [gen-double within]]
            [same.core :refer [ish? with-comparator]]))

(defn factorial
  "Factorial implementation for testing."
  [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn ->logpdf [f]
  (fn [& args]
    (reify distribution/LogPDF
      (logpdf [_ v]
        (apply f (concat args [v]))))))

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
  (dt/gamma-tests (->logpdf ll/gamma)))

(deftest beta-tests
  (dt/beta-tests (->logpdf ll/beta)))

(deftest bernoulli-tests
  (dt/bernoulli-tests (->logpdf ll/bernoulli)))

(deftest cauchy-tests
  (dt/cauchy-tests (->logpdf ll/cauchy)))

(deftest delta-tests
  (dt/delta-tests (->logpdf ll/delta)))

(deftest exponential-tests
  (dt/exponential-tests (->logpdf ll/exponential))

  (checking "exponential will never produce negative values"
            [rate (gen-double -100 100)
             v    (gen-double -100 -0.00001)]
            (is (= ##-Inf (ll/exponential rate v))))

  (checking "rate 0.0 produces #-Inf"
            [v (gen-double -100 100)]
            (is (= ##-Inf (ll/exponential 0.0 v)))))

(deftest laplace-test
  (dt/laplace-tests (->logpdf ll/laplace)))

(deftest gaussian-tests
  (dt/normal-tests (->logpdf ll/gaussian)))

(deftest uniform-tests
  (dt/uniform-tests (->logpdf ll/uniform)))

(deftest student-t-tests
  (dt/student-t-tests (->logpdf ll/student-t)))

(ns gen.distribution.math.log-likelihood-test
  (:require [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [gen.distribution.math.log-likelihood :as ll]))

(def nonzero-double
  (gen/double*
   {:min 0.001 :max 10 :infinite? false :NaN? false}))

(deftest beta-tests
  (checking "(log of the) Beta function is symmetrical"
            [a nonzero-double
             b nonzero-double]
            (is (= (ll/log-beta-fn a b)
                   (ll/log-beta-fn b a)))))

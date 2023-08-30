(ns gen.distribution.kixi-test
  (:require [clojure.test :refer [deftest]]
            [gen.distribution-test :as dt]
            [gen.distribution.kixi :as kixi]))

(deftest bernoulli-tests
  (dt/bernoulli-tests kixi/bernoulli-distribution)
  (dt/bernoulli-gfi-tests kixi/bernoulli))

(deftest beta-tests
  (dt/beta-tests kixi/beta-distribution))

(deftest cauchy-tests
  (dt/cauchy-tests kixi/cauchy-distribution))

(deftest exponential-tests
  (dt/exponential-tests kixi/exponential-distribution))

(deftest uniform-tests
  (dt/uniform-tests kixi/uniform-distribution))

(deftest normal-tests
  (dt/normal-tests kixi/normal-distribution))

(deftest gamma-tests
  (dt/gamma-tests kixi/gamma-distribution))

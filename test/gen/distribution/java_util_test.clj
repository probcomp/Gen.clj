(ns gen.distribution.java-util-test
  (:require [clojure.test :refer [deftest]]
            [gen.distribution-test :as dt]
            [gen.distribution.java-util :as java-util]))

(deftest bernoulli-tests
  (dt/bernoulli-tests java-util/bernoulli-distribution)
  (dt/bernoulli-gfi-tests java-util/bernoulli))

(deftest binomial-tests
  (dt/binomial-tests java-util/binomial-distribution))

(deftest uniform-tests
  (dt/uniform-tests java-util/uniform-distribution))

(deftest normal-tests
  (dt/normal-tests java-util/normal-distribution))

(ns gen.distribution.simple-statistics-test
  (:require [clojure.test :refer [is deftest]]
            [gen.distribution.simple-statistics :as ss]))

(deftest basic-test
  (is (= 1 (ss/bernoulli 0.3))))

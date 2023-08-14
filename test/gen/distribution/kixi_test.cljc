(ns gen.distribution.kixi-test
  (:require [clojure.test :refer [is deftest]]
            [gen.distribution.kixi :as k]))

(deftest basic-test
  (is (= 1 (k/bernoulli 0.3))))

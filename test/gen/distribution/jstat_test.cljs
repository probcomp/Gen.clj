(ns gen.distribution.jstat-test
  (:require [clojure.test :refer [is deftest]]
            [gen.distribution.jstat :as j]))

(deftest basic-test
  (is (= 1 (js-keys (j/bernoulli 0.3)))))

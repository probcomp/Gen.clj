(ns gen.test-check-util
  "Utilities for using Gen distributions and types with test.check."
  (:require [clojure.test.check.generators :as gen]))

(defn within
  "Returns a function that tests whether two values are within `eps` of each
  other."
  [^double eps]
  (fn [^double x ^double y]
    (< (Math/abs (- x y)) eps)))

(defn gen-double
  "Returns a generator that produces numerical doubles between `min` and
  `max` (inclusive)."
  [min max]
  (gen/double*
   {:min min :max max :infinite? false :NaN? false}))

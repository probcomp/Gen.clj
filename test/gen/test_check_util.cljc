(ns gen.test-check-util
  "Utilities for using Gen distributions and types with test.check."
  (:require [clojure.test.check.generators :as gen]))

(defn gen-double
  "Returns a generator that produces numerical doubles between `min` and
  `max` (inclusive)."
  [min max]
  (gen/double*
   {:min min :max max :infinite? false :NaN? false}))

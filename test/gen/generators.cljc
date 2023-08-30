(ns gen.generators
  "test.check generators for Gen.clj types and other useful combinators."
  (:require [clojure.test.check.generators :as gen]))

(defn gen-double [min max]
  (gen/double*
   {:min min :max max :infinite? false :NaN? false}))

(ns gen.generators
  "test.check generators for Gen.clj types and other useful combinators."
  (:require [clojure.test.check.generators :as gen]
            [gen.choicemap :as choicemap]))

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

(def reasonable-double
  "Generator that returns non-messy doubles in the range [-100, 100]."
  (gen-double -100 100))

;; ## IChoiceMap generators

(defn gen-choice
  "Returns a generator that produces [[gen.choicemap/Choice]] instances."
  ([] (gen-choice gen/any-equatable))
  ([v-gen] (gen/fmap choicemap/->Choice v-gen)))

(defn gen-dynamic-choicemap
  "Returns a generator that produces [[gen.choicemap/Choice]] instances."
  ([] (gen-dynamic-choicemap gen/keyword reasonable-double))
  ([v-gen] (gen-dynamic-choicemap gen/keyword v-gen))
  ([k-gen v-gen]
   (gen/fmap choicemap/choicemap
             (gen/map k-gen v-gen))))

(defn gen-vector-choicemap
  "Returns a generator that produces [[gen.choicemap/Choice]] instances."
  ([] (gen-vector-choicemap reasonable-double))
  ([entry-gen]
   (gen/fmap choicemap/choicemap
             (gen/vector entry-gen))))

(defn gen-choicemap
  "Returns a generator that produces nested [[gen.choicemap/IChoiceMap]] instances
  composed of vectors, maps and leaves.

  Leaf contents are generated with `v-gen`."
  ([] (gen-choicemap reasonable-double))
  ([v-gen]
   (gen/recursive-gen
    (fn [inner]
      (gen/one-of
       [(gen-dynamic-choicemap inner)
        (gen-vector-choicemap inner)]))
    (gen-choice v-gen))))

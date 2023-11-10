(ns gen.choicemap-test
  "Tests for the [[gen.choicemap]] namespace."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.choicemap :as choicemap]
            [gen.generators :as generators]))

(defn common-tests [m]
  (is (choicemap/choicemap? m)
      "input is in fact a choicemap.")

  (is (= (choicemap/get-values-shallow m)
         (reduce-kv (fn [acc k v]
                      (if (choicemap/has-value? v)
                        (assoc acc k (choicemap/get-value v))
                        acc))
                    {}
                    (choicemap/get-submaps-shallow m)))
      "get-values-shallow is the correct subset of get-submaps-shallow")

  (is (every? true?
              (for [[k _] (choicemap/get-values-shallow m)]
                (choicemap/has-value? m k)))
      "the outer choicemap respects 2-arity has-value?")

  (is (every? true?
              (for [[k leaf] (choicemap/get-values-shallow m)]
                (= leaf (choicemap/get-value m k))))
      "everything in get-values-shallow is unwrapped."))

(defn leaf-tests
  "Tests for leaf-shaped choicemap instances."
  [choice]
  (common-tests choice)

  (is (empty? (choicemap/get-submaps-shallow choice))
      "leaves should never return submaps.")

  (is (empty? (choicemap/get-values-shallow choice))
      "leaves should never return nested values.")

  (is (true? (choicemap/has-value? choice))
      "leaves always have values.")

  (is (= choice (choicemap/->Choice
                 (choicemap/get-value choice)))
      "round-tripping through get-value and constructor ==
                identity"))

(defn node-tests
  "Tests for node-shaped choicemap instances."
  [m]
  (common-tests m)

  (is (false? (choicemap/has-value? m))
      "leaves should never return submaps.")

  (is (nil? (choicemap/get-value m))
      "leaves should never return nested values."))

(deftest interface-tests
  (checking "anything else is NOT a choicemap"
            [x gen/any-equatable]
            (is (not (choicemap/choicemap? x)))))

(deftest choice-tests
  (checking "interface for leaves" 100
            [choice (generators/gen-choice)]
            (leaf-tests choice)))

(deftest empty-choicemap-tests
  (node-tests choicemap/EMPTY))

(deftest dynamic-choicemap-tests
  (checking "interface for maps" 100
            [m (generators/gen-dynamic-choicemap)]
            (node-tests m)))

(deftest vector-choicemap-tests
  (checking "interface for vectors" 100
            [m (generators/gen-vector-choicemap)]
            (node-tests m)))

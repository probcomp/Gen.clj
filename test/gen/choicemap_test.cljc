(ns gen.choicemap-test
  "Tests for the [[gen.choicemap]] namespace and the various base implementations
  that live there."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.array-test :as array-test]
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

  (checking "get-submap always returns empty for leaves" 100
            [k gen/keyword]
            (is (empty?
                 (choicemap/get-submap choice k))))

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

  (let [submaps (choicemap/get-submaps-shallow m)]
    (if (seq submaps)
      (is (seq m)
          "non-empty submaps == non-empty m")
      (is (empty? m)
          "empty submaps == empty? m ")))

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
            (leaf-tests choice))

  (checking "leaves round-trip through array" 100
            [choice (generators/gen-choice)]
            (array-test/array-round-trip choice))

  (checking "leaves are never empty" 100
            [choice (generators/gen-choice)]
            (is (not (choicemap/empty? choice))))

  (checking "string rep" 100
            [choice (generators/gen-choice)]
            (is (= (str "#gen/choice "
                        (pr-str
                         (choicemap/get-value choice)))
                   (str choice)))))

(deftest empty-choicemap-tests
  (node-tests choicemap/EMPTY)
  (array-test/array-round-trip choicemap/EMPTY)

  (is (= "#gen/choicemap {}" (str choicemap/EMPTY))
      "string rep test")

  (is (= choicemap/EMPTY choicemap/EMPTY)
      "empty is equal only to itself")

  (is (empty? choicemap/EMPTY) "clojure's empty?")

  (is (= choicemap/EMPTY (empty choicemap/EMPTY))
      "clojure's empty works")

  (checking "not equal to anything else" 100
            [v gen/any-equatable]
            (is (not= choicemap/EMPTY v))
            #?(:clj
               (is (not (.equals choicemap/EMPTY v)))))

  (checking "no entries" 100 [k gen/keyword]
            (is (= choicemap/EMPTY
                   (choicemap/get-submap choicemap/EMPTY k))))

  (is (= choicemap/EMPTY (choicemap/choicemap))
      "no-arity constructor returns empty choicemap.")

  (checking "assoc onto empty"
            [k gen/keyword v gen/small-integer]
            (is (= (choicemap/choicemap {k v})
                   (assoc choicemap/EMPTY k v)))))

(deftest dynamic-choicemap-tests
  (checking "(comp ->map choicemap) == identity" 100
            [m (gen/map gen/keyword
                        (gen/recursive-gen
                         (fn [inner]
                           (gen/map gen/keyword inner))
                         gen/small-integer))]
            (let [cm (choicemap/choicemap m)]
              (is (choicemap/choicemap? cm))

              (is (= (count m)
                     (count cm))
                  "choicemaps preserve count.")

              (is (= m (choicemap/->map cm))
                  "(comp ->map choicemap) == identity")))

  (checking "interface for maps" 100
            [m (generators/gen-dynamic-choicemap)]
            (node-tests m)
            (is (empty? (empty m))
                "empty returns an empty choicemap."))

  (checking "Interface tests for choice maps"
            [m (generators/gen-dynamic-choicemap)]
            (is (= m (choicemap/choicemap
                      (zipmap (keys m) (vals m))))
                "keys and vals work correctly"))

  (checking "dcm round-trips through array" 100
            [m (generators/gen-dynamic-choicemap)]
            (array-test/array-round-trip m))

  (checking "metadata support" 100
            [cm (generators/gen-dynamic-choicemap)
             m (gen/map gen/keyword gen/any)]
            (is (= m (meta
                      (with-meta cm m)))))

  (checking "invoke == get-submap" 100
            [m (generators/gen-dynamic-choicemap)]
            (is (every?
                 true?
                 (for [k (keys m)]
                   (and (choicemap/has-submap? m k)
                        (= (choicemap/get-submap m k)
                           (m k)
                           (m k ::missing)))))))

  (checking "not-found arity" 100
            [m (generators/gen-dynamic-choicemap)]
            (is (= ::not-found
                   (m ::not-present ::not-found))))

  (checking "assoc matches merge"
            [m (generators/gen-dynamic-choicemap)
             v gen/small-integer]
            (is (= (assoc m ::key v)
                   (choicemap/merge m
                                    (choicemap/choicemap
                                     {::key v})))))

  (testing "get-values-shallow vs get-submaps-shallow"
    (let [k2m (choicemap/choicemap
               {:a "b"})
          m (choicemap/choicemap
             {:k1 "1"
              :k2 k2m})]
      (is (= {:k1 "1"}
             (choicemap/get-values-shallow m))
          "get-values-shallow returns unwrapped.")

      (is (= {:k1 (choicemap/choicemap "1")
              :k2 k2m}
             (choicemap/get-submaps-shallow m))
          "get-submaps-shallow creates a map from all entries.")))

  (checking "conj implementation" 100
            [m (generators/gen-dynamic-choicemap)
             k gen/keyword
             v gen/small-integer]
            (is (= (conj m [k v])
                   (conj m {k v})
                   (assoc m k v)))))

(deftest vector-choicemap-tests
  (checking "choicemaps preserve count" 100
            [v (gen/vector
                (gen/recursive-gen
                 (fn [inner]
                   (gen/vector inner))
                 gen/small-integer))]
            (let [cm (choicemap/choicemap v)]
              (is (choicemap/choicemap? cm))

              (is (= (count v)
                     (count cm)))))

  (checking "interface for vectors" 100
            [v (generators/gen-vector-choicemap)]
            (node-tests v)
            (is (empty? (empty v))
                "empty returns an empty choicemap."))

  (checking "interface for vectors" 100
            [v (generators/gen-vector-choicemap)]
            (is (= v (choicemap/choicemap
                      (into [] (seq v))))
                "vector choicemap round-trips"))

  (checking "vcm round-trips through array" 100
            [v (generators/gen-vector-choicemap)]
            (array-test/array-round-trip v))

  (checking "metadata support" 100
            [v (generators/gen-vector-choicemap)
             m (gen/map gen/keyword gen/any)]
            (is (= m (meta
                      (with-meta v m)))))

  (checking "invoke == get-submap" 100
            [v (generators/gen-vector-choicemap)]
            (is (every?
                 true?
                 (for [k (range (count v))]
                   (and (choicemap/has-submap? v k)
                        (= (choicemap/get-submap v k)
                           (choicemap/get-submap v k)
                           (v k)
                           (v k ::missing)))))))

  (checking "not-found arity" 100
            [v (generators/gen-vector-choicemap)]
            (is (= ::not-found
                   (v ::not-present ::not-found))))

  (testing "get-values-shallow vs get-submaps-shallow"
    (let [v (choicemap/choicemap [1 [2 3]])]
      (is (= {0 1}
             (choicemap/get-values-shallow v))
          "get-values-shallow returns unwrapped.")

      (is (= {0 (choicemap/choicemap 1)
              1 (choicemap/choicemap [2 3])}
             (choicemap/get-submaps-shallow v))
          "get-submaps-shallow creates a map from all entries.")))

  (testing "conj implementation"
    (is (= (choicemap/choicemap [1 2 3 4])
           (conj (choicemap/choicemap [1 2 3]) 4))))

  (testing "assoc implementation on vectors"
    (is (= (choicemap/choicemap [0 1 1])
           (-> (choicemap/choicemap [1 1 1])
               (assoc 0 0)))
        "internal assoc")

    (is (= (choicemap/choicemap [1 1 1 0])
           (-> (choicemap/choicemap [1 1 1])
               (assoc 3 0)))
        "assoc at the end keeps vector")

    (is (= (choicemap/choicemap {0 1
                                 1 1
                                 2 1
                                 :a "b"})
           (-> (choicemap/choicemap [1 1 1])
               (assoc :a "b")))
        "assoc at some other location converts to map")))

(deftest api-tests
  (checking "->map is identity on non-choicemaps" 100
            [v gen/any-equatable]
            (is (= v (choicemap/->map v))))

  (testing "assoc-in"
    (checking "assoc matches assoc-in for single k"
              100
              [k gen/keyword
               v gen/any-equatable]
              (is (= (assoc-in choicemap/EMPTY [k] v)
                     (assoc choicemap/EMPTY k v))))

    (is (= (choicemap/choicemap {:k1 {:inner 10}
                                 :k2 2})
           (choicemap/assoc-in
            (choicemap/choicemap {:k2 2})
            [:k1 :inner]
            10))
        "assoc-in works great on nested values")

    (is (thrown?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         (choicemap/assoc-in
          (choicemap/choicemap {:k1 1 :k2 2})
          [:k1 :inner]
          10))
        "assoc-in won't traverse past a value"))

  (testing "empty?"
    (is (choicemap/empty? []))
    (is (choicemap/empty? {})))

  (testing "merge"
    (is (= choicemap/EMPTY (choicemap/merge))
        "no-arity returns empty.")

    (checking "single-arity acts as identity" 100
              [m (generators/gen-dynamic-choicemap)]
              (is (= m (choicemap/merge m))
                  "single arity acts as identity."))

    (is (thrown?
         #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
         (choicemap/merge
          (choicemap/choicemap {:k 1})
          (choicemap/choicemap {:k 2})))
        "clashing values aren't allowed in a merge.")

    (is (= (choicemap/choicemap {:a 1 :b 1 :c 1})
           (choicemap/merge
            (choicemap/choicemap {:a 1})
            (choicemap/choicemap {:b 1})
            (choicemap/choicemap {:c 1}))
           (choicemap/merge
            (choicemap/choicemap {:a 1})
            (choicemap/merge
             (choicemap/choicemap {:b 1})
             (choicemap/choicemap {:c 1})))
           (choicemap/merge
            (choicemap/merge
             (choicemap/choicemap {:a 1})
             (choicemap/choicemap {:b 1}))
            (choicemap/choicemap {:c 1})))
        "merge is associative, provided there are no clashes")))

(ns gen.dynamic.choice-map-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.choice-map :as choice-map]
            [gen.dynamic.choice-map :as dynamic.choice-map]))

(def gen-choice-map
  (comp (partial gen/fmap dynamic.choice-map/choice-map)
        gen/map))

(deftest choice
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice nil)))
  (is (dynamic.choice-map/choice? #gen/choice nil))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice :x)))
  (is (dynamic.choice-map/choice? #gen/choice :x))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice [:x])))
  (is (dynamic.choice-map/choice? #gen/choice [:x]))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice {:x 0})))
  (is (dynamic.choice-map/choice? #gen/choice {:x 0}))
  (is (not (dynamic.choice-map/choice? nil)))
  (is (not (dynamic.choice-map/choice? :x))))

(deftest choice-map?
  (is (dynamic.choice-map/choice-map? #gen/choice-map {}))
  (is (not (dynamic.choice-map/choice-map? {}))))

(deftest choice-map-value
  (is (= nil (choice-map/value #gen/choice nil)))
  (is (= :x (choice-map/value #gen/choice :x))))

(deftest empty?
  (is (clojure/empty? (dynamic.choice-map/choice-map)))
  (is (clojure/empty? #gen/choice-map {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? #gen/choice-map {:x 0}))))

(defn iterable-seq [^Iterable iter]
  (when (.hasNext iter)
    (lazy-seq
     (cons (.next iter)
           (iterable-seq iter)))))

(deftest interface-tests
  (checking "Interface tests for choice maps"
            [m (gen-choice-map gen/keyword gen/any-equatable)]
            (is (= (seq m)
                   (iterable-seq
                    (.iterator ^Iterable m)))
                "iterator impl matches seq")

            (is (= m (dynamic.choice-map/choice-map
                      (zipmap (keys m) (vals m))))
                "keys and vals work correctly")))

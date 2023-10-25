(ns gen.dynamic.choice-map-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.dynamic.choice-map :as choice-map]))

(def gen-choice-map
  (comp (partial gen/fmap choice-map/choice-map)
        gen/map))

(deftest choice
  (is (choice-map/choice? (choice-map/choice nil)))
  (is (choice-map/choice? (choice-map/choice :x)))
  (is (choice-map/choice? (choice-map/choice [:x])))
  (is (choice-map/choice? (choice-map/choice {:x 0})))
  (is (not (choice-map/choice? nil)))
  (is (not (choice-map/choice? :x))))

(deftest choice-map?
  (is (choice-map/choice-map? {})))

(deftest choice-map-value
  ;; TODO turn into a generative test.
  (is (= nil (choice-map/value (choice-map/choice nil))))
  (is (= :x (choice-map/value (choice-map/choice :x)))))

(deftest empty?
  (is (clojure/empty? (choice-map/choice-map)))
  (is (clojure/empty? {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? {:x 0}))))

(defn iterable-seq [^Iterable iter]
  (when (.hasNext iter)
    (lazy-seq
     (cons (.next iter)
           (iterable-seq iter)))))

(deftest interface-tests
  (checking "Interface tests for choice maps"
            [m (gen-choice-map gen/keyword gen/any-equatable)]
            #?(:clj
               (is (= (seq m)
                      (iterable-seq
                       (.iterator ^Iterable m)))
                   "iterator impl matches seq"))

            (is (= m (choice-map/choice-map
                      (zipmap (keys m) (vals m))))
                "keys and vals work correctly")))

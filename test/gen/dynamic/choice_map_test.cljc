(ns gen.dynamic.choicemap-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.dynamic.choicemap :as choicemap]))

(def gen-choicemap
  (comp (partial gen/fmap choicemap/choicemap)
        gen/map))

(deftest choice
  (is (choicemap/choice? (choicemap/choice nil)))
  (is (choicemap/choice? #gen/choice nil))
  (is (choicemap/choice? (choicemap/choice :x)))
  (is (choicemap/choice? #gen/choice :x))
  (is (choicemap/choice? (choicemap/choice [:x])))
  (is (choicemap/choice? #gen/choice [:x]))
  (is (choicemap/choice? (choicemap/choice {:x 0})))
  (is (choicemap/choice? #gen/choice {:x 0}))
  (is (not (choicemap/choice? nil)))
  (is (not (choicemap/choice? :x))))

(deftest choicemap?
  (is (choicemap/choicemap? #gen/choicemap {}))
  (is (not (choicemap/choicemap? {}))))

(deftest choicemap-value
  (is (= nil (choicemap/unwrap #gen/choice nil)))
  (is (= :x (choicemap/unwrap #gen/choice :x))))

(deftest empty?
  (is (clojure/empty? (choicemap/choicemap)))
  (is (clojure/empty? #gen/choicemap {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? #gen/choicemap {:x 0}))))

(defn iterable-seq [^Iterable iter]
  (when (.hasNext iter)
    (lazy-seq
     (cons (.next iter)
           (iterable-seq iter)))))

(deftest interface-tests
  (checking "Interface tests for choice maps"
            [m (gen-choicemap gen/keyword gen/any-equatable)]
            #?(:clj
               (is (= (seq m)
                      (iterable-seq
                       (.iterator ^Iterable m)))
                   "iterator impl matches seq"))

            (is (= m (choicemap/choicemap
                      (zipmap (keys m) (vals m))))
                "keys and vals work correctly")))

(ns gen.dynamic.choice-map-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [gen.choice-map :as choice-map]
            [gen.dynamic.choice-map :as dynamic.choice-map]))

(deftest choice
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice nil)))
  (is (dynamic.choice-map/choice? nil))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice :x)))
  (is (dynamic.choice-map/choice? :x))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice [:x])))
  (is (dynamic.choice-map/choice? [:x]))
  (is (dynamic.choice-map/choice? (dynamic.choice-map/choice {:x 0})))
  (is (dynamic.choice-map/choice? {:x 0})))

(deftest choice-map?
  (is (dynamic.choice-map/choice-map? #gen/choice-map {}))
  (is (not (dynamic.choice-map/choice-map? {}))))

(deftest choice-map-value
  (is (= nil (choice-map/value nil)))
  (is (= :x (choice-map/value :x))))

(deftest empty?
  (is (clojure/empty? (dynamic.choice-map/choice-map)))
  (is (clojure/empty? #gen/choice-map {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? #gen/choice-map {:x 0}))))

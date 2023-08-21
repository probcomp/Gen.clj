(ns gen.choice-map-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [gen.choice-map :as choice-map]))

(deftest choice
  (is (choice-map/choice? (choice-map/choice nil)))
  (is (choice-map/choice? #gen/choice nil))
  (is (choice-map/choice? (choice-map/choice :x)))
  (is (choice-map/choice? #gen/choice :x))
  (is (choice-map/choice? (choice-map/choice [:x])))
  (is (choice-map/choice? #gen/choice [:x]))
  (is (choice-map/choice? (choice-map/choice {:x 0})))
  (is (choice-map/choice? #gen/choice {:x 0})))

(deftest choice-map?
  (is (choice-map/choice-map? #gen/choice-map {}))
  (is (not (choice-map/choice-map? {}))))

(deftest empty?
  (is (clojure/empty? (choice-map/make)))
  (is (clojure/empty? #gen/choice-map {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? #gen/choice-map {:x 0}))))

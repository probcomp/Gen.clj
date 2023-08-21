(ns gen.choice-map-test
  (:refer-clojure :exclude [empty empty?])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [gen.choice-map :as choice-map]))

(deftest choice-tests
  (is (choice-map/choice? (choice-map/choice nil)))
  (is (choice-map/choice? #gen/choice nil))
  (is (choice-map/choice? (choice-map/choice :x)))
  (is (choice-map/choice? #gen/choice :x))
  (is (choice-map/choice? (choice-map/choice [:x])))
  (is (choice-map/choice? #gen/choice [:x]))
  (is (choice-map/choice? (choice-map/choice {:x 0})))
  (is (choice-map/choice? #gen/choice {:x 0})))

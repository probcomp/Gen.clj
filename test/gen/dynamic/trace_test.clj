(ns gen.dynamic.trace-test
  (:refer-clojure :exclude [empty? get keys seq update vals])
  (:require [clojure.core :as clojure]
            [clojure.test :refer [deftest is]]
            [gen]
            [gen.dynamic :refer [gen]]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.dynamic.trace :as dynamic.trace]
            [gen.trace :as trace]))

(defn choice-trace
  [x]
  (reify trace/Choices
    (choices [_]
      (dynamic.choice-map/choice x))))

(deftest empty?
  (let [trace (dynamic.trace/trace (gen []) [])]
    (is (clojure/empty? trace))))

(deftest gf
  (let [gf (gen [])]
    (is (= gf (trace/gf (dynamic.trace/trace gf []))))))

(deftest args
  (is (= [] (trace/args (dynamic.trace/trace (gen []) []))))
  (is (= [0] (trace/args (dynamic.trace/trace (gen [x] x) [0]))))
  (is (= [0 1] (trace/args (dynamic.trace/trace (gen [x y] (+ x y)) [0 1])))))

(deftest call-position
  (let [trace (dynamic.trace/trace (gen []) [])]
    (is (nil? (trace :addr))))
  (let [trace (-> (dynamic.trace/trace (gen []) [])
                  (dynamic.trace/assoc-subtrace :addr (choice-trace :x)))]
    (is (= :x (trace :addr)))))

(deftest keys
  (is (= #{:addr}
         (-> (dynamic.trace/trace (gen []) [])
             (dynamic.trace/assoc-subtrace :addr (choice-trace :x))
             (clojure/keys)
             (set))))
  (is (= #{:addr1 :addr2}
         (-> (dynamic.trace/trace (gen []) [])
             (dynamic.trace/assoc-subtrace :addr1 (choice-trace :x))
             (dynamic.trace/assoc-subtrace :addr2 (choice-trace :y))
             (clojure/keys)
             (set)))))

(deftest vals
  (is (= #{:x}
         (-> (dynamic.trace/trace (gen []) [])
             (dynamic.trace/assoc-subtrace :addr (choice-trace :x))
             (clojure/vals)
             (set))))
  (is (= #{:x :y}
         (-> (dynamic.trace/trace (gen []) [])
             (dynamic.trace/assoc-subtrace :addr1 (choice-trace :x))
             (dynamic.trace/assoc-subtrace :addr2 (choice-trace :y))
             (clojure/vals)
             (set)))))

(deftest seq
  (let [trace (-> (dynamic.trace/trace (gen []) [])
                  (dynamic.trace/assoc-subtrace :addr0 (choice-trace :x))
                  (dynamic.trace/assoc-subtrace :addr1 (choice-trace :y))
                  (dynamic.trace/assoc-subtrace :addr2 (choice-trace :z)))]
    (is (every? map-entry? (clojure/seq trace)))))

(deftest get
  (let [trace (dynamic.trace/trace (gen []) [])]
    (is (nil? (clojure/get trace :addr))))
  (let [trace (-> (dynamic.trace/trace (gen []) [])
                  (dynamic.trace/assoc-subtrace :addr (choice-trace :x)))]
    (is (= :x (clojure/get trace :addr)))))

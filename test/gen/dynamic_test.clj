(ns gen.dynamic-test
  (:require [clojure.math :as math]
            [clojure.test :refer [deftest is]]
            [gen]
            [gen.distribution.fastmath :as d]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

(deftest call
  (is (nil? ((gen []))))
  (is (= 0 ((gen [] 0))))
  (is (nil? ((gen [_]) 0)))
  (is (= 0 ((gen [x] x) 0))))

(deftest gf
  (let [gf (gen [])
        trace (gf/simulate gf [])]
    (is (= gf (trace/gf trace)))))

(deftest trace-form?-false
  (is (not (dynamic/trace-form? '())))
  (is (not (dynamic/trace-form? '(trace)))))

(deftest trace-form?-true
  (is (dynamic/trace-form? `(gen/trace)))
  (is (dynamic/trace-form? `(gen/trace :x)))
  (is (dynamic/trace-form? `(gen/trace ~'x)))
  (is (dynamic/trace-form? `(gen/trace :x :y))))

(deftest valid-trace-form?-false
  (is (not (dynamic/valid-trace-form? `(gen/trace :addr d/bernoulli))))
  (is (not (dynamic/valid-trace-form? `(gen/trace :addr d/bernoulli [])))))

(deftest valid-trace-form?-true
  (is (dynamic/valid-trace-form? `(gen/trace :addr (d/bernoulli))))
  (is (dynamic/valid-trace-form? `(gen/trace :addr (d/bernoulli 0.5))))
  (is (dynamic/valid-trace-form? `(gen/trace :addr ((gen [])))))
  (is (dynamic/valid-trace-form? `(gen/trace :addr ((gen [x]) 0)))))

(deftest trace-args
  (let [n 0]
    (is (= [n] (trace/args (gf/simulate (gen [_]) [n]))))))

(deftest score
  (is (= 0.5 (math/exp (trace/score (gf/simulate d/bernoulli [0.5])))))
  (let [trace (gf/simulate (gen []
                             (gen/trace :addr (d/bernoulli 0.5)))
                           [])]
    (is (= 0.5 (math/exp (trace/score trace))))))

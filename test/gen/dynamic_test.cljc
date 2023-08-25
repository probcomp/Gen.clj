(ns gen.dynamic-test
  (:require [clojure.math :as math]
            [clojure.test :refer [deftest is]]
            [gen]
            [gen.distribution.kixi :as d]
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
  (is (= [0] (trace/args (gf/simulate (gen [& _]) [0]))))
  (is (= [0 1] (trace/args (gf/simulate (gen [& _]) [0 1])))))

(deftest simulate-trace
  (let [gf (gen [] (gen/trace :addr (d/bernoulli)))
        trace (gf/simulate gf [])
        choice-map (trace/choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choice-map))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choice-map)))))

(deftest simulate-splice
  (let [gf0 (gen [] (gen/trace :addr (d/bernoulli)))
        gf1 (gen [] (gen/splice (gf0)))
        trace (gf/simulate gf1 [])
        choice-map (trace/choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choice-map))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choice-map)))))

(deftest generate-trace-trace
  (let [gf (gen [] (gen/trace :addr (d/bernoulli)))
        trace (:trace (gf/generate gf []))
        choice-map (trace/choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choice-map))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choice-map)))))

(deftest generate-splice-trace
  (let [gf0 (gen [] (gen/trace :addr (d/bernoulli)))
        gf1 (gen [] (gen/splice (gf0)))
        trace (:trace (gf/generate gf1 []))
        choice-map (trace/choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choice-map))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choice-map)))))

(deftest generate-call-trace
  (let [gf0 (gen [] (gen/trace :addr (d/bernoulli)))
        gf1 (gen [] (gf0))
        trace (:trace (gf/generate gf1 []))
        choice-map (trace/choices trace)]
    (is (empty? trace))
    (is (empty? choice-map))))

(deftest generate-call-splice
  (let [gf0 (gen [] (gen/splice (d/bernoulli)))
        gf1 (gen [] (gf0))
        trace (:trace (gf/generate gf1 []))
        choice-map (trace/choices trace)]
    (is (empty? trace))
    (is (empty? choice-map))))

(deftest score
  (is (= 0.5 (math/exp (trace/score (gf/simulate d/bernoulli [0.5])))))
  (let [trace (gf/simulate (gen []
                             (gen/trace :addr (d/bernoulli 0.5)))
                           [])]
    (is (= 0.5 (math/exp (trace/score trace))))))

(deftest update-discard-yes
  (let [gf (gen []
             (gen/trace :discarded (d/bernoulli 0)))]
    (is (= #gen/choice-map {:discarded false}
           (-> (gf/simulate gf [])
               (trace/update #gen/choice-map {:discarded true})
               (:discard))))))

(deftest update-discard-no
  (let [gf (gen []
             (gen/trace :not-discarded (d/bernoulli 0)))]
    (is (empty? (-> (gf/simulate gf [])
                    (trace/update #gen/choice-map {:discarded true})
                    (:discard))))))

(deftest update-discard-both
  (let [gf (gen []
             (gen/trace :discarded (d/bernoulli 0))
             (gen/trace :not-discarded (d/bernoulli 1)))]
    (is (= #gen/choice-map {:discarded false}
           (-> (gf/simulate gf [])
               (trace/update #gen/choice-map {:discarded true})
               (:discard))))))

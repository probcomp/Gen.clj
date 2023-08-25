(ns gen.distribution.commons-math-test
  (:require [clojure.math :as math]
            [clojure.test :refer [deftest is]]
            [gen.diff :as diff]
            [gen.distribution.commons-math :as d]
            [gen.generative-function :as gf]
            [gen.trace-protocols :as trace]))

(deftest bernoulli-call-no-args
  (is (boolean? (d/bernoulli))))

(deftest bernoulli-call-args
  (is (boolean? (d/bernoulli 0.5))))

(deftest bernoulli-gf
  (is (= d/bernoulli (trace/gf (gf/simulate d/bernoulli [])))))

(deftest bernoulli-args
  (is (= [0.5] (trace/args (gf/simulate d/bernoulli [0.5])))))

(deftest bernoulli-retval
  (is (boolean? (trace/retval (gf/simulate d/bernoulli [0.5])))))

(deftest bernoulli-choices-noargs
  (trace/choices (gf/simulate d/bernoulli [])))

(deftest bernoulli-update-weight
  (is (= 1.0
         (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
             (:trace)
             (trace/update #gen/choice true)
             (:weight)
             (math/exp))))
  (is (= (/ 0.7 0.3)
         (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
             (:trace)
             (trace/update #gen/choice false)
             (:weight)
             (math/exp)))))

(deftest bernoulli-update-discard
  (is (nil?
       (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
           (:trace)
           (trace/update nil)
           (:discard))))
  (is (= #gen/choice true
         (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
             (:trace)
             (trace/update #gen/choice false)
             (:discard)))))

(deftest bernoulli-update-change
  (is (= diff/unknown-change
       (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
           (:trace)
           (trace/update nil)
           (:change))))
  (is (= diff/unknown-change
         (-> (gf/generate d/bernoulli [0.3] #gen/choice true)
             (:trace)
             (trace/update #gen/choice false)
             (:change)))))

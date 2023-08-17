(ns gen.distribution.kixi-test
  (:require [clojure.math :as math]
            [clojure.test :refer [deftest is]]
            [gen]
            [gen.choice-map]
            [gen.diff :as diff]
            [gen.distribution.kixi :as d]
            [gen.dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

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

(def line-model
  (gen [xs]
    ;; We begin by sampling a slope and intercept for the line.  Before we have
    ;; seen the data, we don't know the values of these parameters, so we treat
    ;; them as random choices. The distributions they are drawn from represent our
    ;; prior beliefs about the parameters: in this case, that neither the slope
    ;; nor the intercept will be more than a couple points away from 0.

    (let [slope     (gen/trace :slope (d/normal 0 1))
          intercept (gen/trace :intercept (d/normal 0 2))

          ;; We define a function to compute y for a given x.
          y (fn [x]
              (+ (* slope x)
                 intercept))]

      ;; Given the slope and intercept, we can sample y coordinates for each of
      ;; the x coordinates in our input vector.

      (doseq [[i x] (map vector (range) xs)]
        (gen/trace [:y i] (d/normal (y x) 0.1)))

      ;; Most of the time, we don't care about the return
      ;; value of a model, only the random choices it makes.
      ;; It can sometimes be useful to return something
      ;; meaningful, however; here, we return the function `y`.
      y)))

(def xs (range -5 6))
(def y (line-model xs))
(def trace (gf/simulate line-model [xs]))

(prn y)
(prn trace)

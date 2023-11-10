(ns gen.sci-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer [testing deftest]]
            [gen.sci :as gs]
            [sci.core :as sci]))

(defn eval [form]
  (let [ctx (sci/fork gs/context)]
    (sci/binding [sci/ns @sci/ns
                  sci/out *out*]
      (sci/eval-form ctx form))))

(deftest sci-tests
  (testing "Check that we can evaluate a model inside SCI."
    (eval
     '(do (require '[gen.distribution.kixi :as dist]
                   '[gen.dynamic :as dynamic :refer [gen]]
                   '[gen.generative-function :as gf]
                   '[gen.trace :as trace])
          (def line-model
            (gen [xs]
                 ;; We begin by sampling a slope and intercept for the line.  Before we have
                 ;; seen the data, we don't know the values of these parameters, so we treat
                 ;; them as random choices. The distributions they are drawn from represent our
                 ;; prior beliefs about the parameters: in this case, that neither the slope
                 ;; nor the intercept will be more than a couple points away from 0.

                 (let [slope     (dynamic/trace! :slope dist/normal 0 1)
                       intercept (dynamic/trace! :intercept dist/normal 0 2)

                       ;; We define a function to compute y for a given x.

                       y (fn [x]
                           (+ (* slope x)
                              intercept))]

                   ;; Given the slope and intercept, we can sample y coordinates for each of
                   ;; the x coordinates in our input vector.

                   (doseq [[i x] (map vector (range) xs)]
                     (dynamic/trace! [:y i] dist/normal (y x) 0.1))

                   ;; Most of the time, we don't care about the return
                   ;; value of a model, only the random choices it makes.
                   ;; It can sometimems be useful to return something
                   ;; meaningful, however; here, we return the function `y`.
                   y)))

          (def xs (range -5 6))
          (def y (line-model xs))
          (def trace (gf/simulate line-model [xs]))
          [(trace/get-args trace)
           (trace/get-choices trace)]))))

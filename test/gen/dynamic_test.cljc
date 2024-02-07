(ns gen.dynamic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.choicemap :as choicemap]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

(deftest binding-tests
  (letfn [(f [_] "hi!")]
    (binding [dynamic/*trace* f]
      (is (= f (dynamic/active-trace))
          "active-trace reflects dynamic bindings"))))

(deftest gen-fn-tests
  (is (nil? ((gen [])))
      "no-arity, no return function returns nil on call")

  (checking "round-trip through functions" [x gen/any-equatable]
            (is (= x ((gen [] x)))))

  (checking "round-trip through functions"
            [xs (gen/vector gen/small-integer 5)]
            (let [gf    (gen [a b c d e]
                          (+ a b c d e))
                  trace (gf/simulate gf xs)]
              (is (= gf (trace/get-gen-fn trace))
                  "distribution round trips through the trace.")

              (is (= xs (trace/get-args trace))
                  "args round-trip through the trace.")

              (is (empty? (trace/get-choices trace))
                  "we made no choices!")

              (is (= (apply gf xs)
                     (trace/get-retval trace))
                  "deterministic functions match the retval."))))

(deftest trace-form-tests
  (testing "incorrect trace forms return false."
    (is (not (dynamic/trace-form? '()))
        "no trace call")

    (is (not (dynamic/trace-form? '(g/trace!)))
        "unknown prefix"))

  (testing "proper trace forms return true."
    (is (dynamic/trace-form? `(dynamic/trace!))
        "correct, but trace will fail due to no args.")
    (is (dynamic/trace-form? `(dynamic/trace! :x))
        "address only, we are lenient here!")

    (is (dynamic/trace-form? `(gen.dynamic/trace! ~'x))
        "different blessed prefixes work")

    (is (dynamic/trace-form? '(trace! :x :y))
        "for now, this special symbol works.")))

(deftest gfi-tests
  (testing "subtleties of nested tracing"
    (let [gf    (gen [p] (dynamic/trace! :addr kixi/bernoulli p))
          trace (gf/simulate gf [0.5])]
      (is (= (choicemap/choicemap
              {:addr (trace/get-retval trace)})
             (trace/get-choices trace))
          "trace choices match retval")))

  (testing "trace inside splice should bubble up"
    (let [gf0   (gen [] (dynamic/trace! :addr kixi/bernoulli))
          gf1   (gen [] (dynamic/splice! gf0))
          trace (gf/simulate gf1 [])]
      (is (= (choicemap/choicemap
              {:addr (trace/get-retval trace)})
             (trace/get-choices trace))
          "works for simulate")

      (let [trace (:trace (gf/generate gf1 [] choicemap/EMPTY))]
        (is (= (choicemap/choicemap
                {:addr (trace/get-retval trace)})
               (trace/get-choices trace))
            "with generate"))))

  (testing "trace inside of trace should nest"
    (let [inner (gen [] (dynamic/trace! :inner kixi/bernoulli))
          outer (gen [] (dynamic/trace! :outer inner))
          trace (gf/simulate outer [])]
      (is (= (choicemap/choicemap
              {:outer
               {:inner
                (trace/get-retval trace)}})
             (trace/get-choices trace))
          "with simulate")

      (let [trace (:trace (gf/generate outer [] choicemap/EMPTY))]
        (is (= (choicemap/choicemap
                {:outer
                 {:inner
                  (trace/get-retval trace)}})
               (trace/get-choices trace))
            "with generate"))))

  (testing "explicit untracing"
    (let [inner (gen [] (dynamic/trace! :addr kixi/bernoulli))
          outer (gen [] (dynamic/untraced (inner)))
          trace (:trace (gf/generate outer []))]
      (is (empty?
           (trace/get-choices trace))
          "untraced turns off tracing")))

  (testing "implicit untraced randomness"
    (let [inner (gen [] (dynamic/trace! :addr kixi/bernoulli))
          outer (gen [] (inner))
          trace (:trace (gf/generate outer []))]
      (is (empty?
           (trace/get-choices trace))
          "function calls induce untraced randomness.")))

  (testing "generate-call-splice"
    (let [inner (gen [] (kixi/bernoulli))
          outer (gen [] (inner))
          trace (:trace (gf/generate outer []))]
      (is (empty?
           (trace/get-choices trace))))))

(deftest score
  (let [trace (gf/simulate (gen []
                             (dynamic/trace! :addr kixi/bernoulli 0.5))
                           [])]
    (is (= 0.5 (Math/exp
                (trace/get-score trace))))))

(deftest update-discard-yes
  (let [gf (gen []
             (dynamic/trace! :discarded kixi/bernoulli 0))]
    (is (= #gen/choicemap {:discarded false}
           (-> (gf/simulate gf [])
               (trace/update {:discarded true})
               (:discard))))))

(deftest update-discard-no
  (let [gf (gen []
             (dynamic/trace! :not-discarded kixi/bernoulli 0))]
    (try (-> (gf/simulate gf [])
             (trace/update {:discarded true}))
         (catch #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) e
           (is (= {:unvisited [:discarded]}
                  (ex-data e)))))))

(deftest update-discard-both
  (let [gf (gen []
             (dynamic/trace! :discarded kixi/bernoulli 0)
             (dynamic/trace! :not-discarded kixi/bernoulli 1))]
    (is (= #gen/choicemap {:discarded false}
           (-> (gf/simulate gf [])
               (trace/update {:discarded true})
               (:discard))))))

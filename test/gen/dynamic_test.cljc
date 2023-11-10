(ns gen.dynamic-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.choicemap :as choicemap]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

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

(deftest trace-form?-false
  (is (not (dynamic/trace-form? '())))
  (is (not (dynamic/trace-form? '(trace)))))

(deftest trace-form?-true
  (is (dynamic/trace-form? `(dynamic/trace!)))
  (is (dynamic/trace-form? `(dynamic/trace! :x)))
  (is (dynamic/trace-form? `(dynamic/trace! ~'x)))
  (is (dynamic/trace-form? `(dynamic/trace! :x :y))))

(deftest trace-args
  (is (= [0] (trace/get-args (gf/simulate (gen [& _]) [0]))))
  (is (= [0 1] (trace/get-args (gf/simulate (gen [& _]) [0 1])))))

(deftest simulate-trace
  (let [gf (gen [] (dynamic/trace! :addr kixi/bernoulli))
        trace (gf/simulate gf [])
        choicemap (trace/get-choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choicemap))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choicemap)))))

(deftest simulate-splice
  (let [gf0 (gen [] (dynamic/trace! :addr kixi/bernoulli))
        gf1 (gen [] (dynamic/splice! gf0))
        trace (gf/simulate gf1 [])
        choicemap (trace/get-choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choicemap))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choicemap)))))

(deftest generate-trace-trace
  (let [gf (gen [] (dynamic/trace! :addr kixi/bernoulli))
        trace (:trace (gf/generate gf []))
        choicemap (trace/get-choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choicemap))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choicemap)))))

(deftest generate-splice-trace
  (let [gf0 (gen [] (dynamic/trace! :addr kixi/bernoulli))
        gf1 (gen [] (dynamic/splice! gf0))
        trace (:trace (gf/generate gf1 []))
        choicemap (trace/get-choices trace)]
    (is (= #{:addr} (set (keys trace))))
    (is (= #{:addr} (set (keys choicemap))))
    (is (boolean? (:addr trace)))
    (is (boolean? (:addr choicemap)))))

(deftest generate-call-trace
  (let [gf0 (gen [] (dynamic/trace! :addr kixi/bernoulli))
        gf1 (gen [] (dynamic/untraced (gf0)))
        trace (:trace (gf/generate gf1 []))
        choicemap (trace/get-choices trace)]
    (is (empty? trace))
    (is (empty? choicemap))))

(deftest generate-call-splice
  (let [gf0 (gen [] (kixi/bernoulli))
        gf1 (gen [] (gf0))
        trace (:trace (gf/generate gf1 []))
        choicemap (trace/get-choices trace)]
    (is (empty? trace))
    (is (empty? choicemap))))

(deftest score
  (is (= 0.5 (Math/exp (trace/get-score (gf/simulate kixi/bernoulli [0.5])))))
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
               (trace/update #gen/choicemap {:discarded true})
               (:discard))))))

(deftest update-discard-no
  (let [gf (gen []
                (dynamic/trace! :not-discarded kixi/bernoulli 0))]
    (is (empty? (-> (gf/simulate gf [])
                    (trace/update #gen/choicemap {:discarded true})
                    (:discard))))))

(deftest update-discard-both
  (let [gf (gen []
                (dynamic/trace! :discarded kixi/bernoulli 0)
                (dynamic/trace! :not-discarded kixi/bernoulli 1))]
    (is (= #gen/choicemap {:discarded false}
           (-> (gf/simulate gf [])
               (trace/update #gen/choicemap {:discarded true})
               (:discard))))))

;; ## ChoiceMap Tets

(def gen-choicemap
  (comp (partial gen/fmap choicemap/choicemap)
        gen/map))

(deftest choicemap?
  (is (choicemap/choicemap? #gen/choicemap {}))
  (is (not (choicemap/choicemap? {}))))

(deftest choicemap-value
  (is (= nil (choicemap/unwrap #gen/choice nil)))
  (is (= :x (choicemap/unwrap #gen/choice :x))))

(deftest empty?
  (is (clojure/empty? (choicemap/choicemap)))
  (is (clojure/empty? #gen/choicemap {}))
  #_{:clj-kondo/ignore [:not-empty?]}
  (is (not (clojure/empty? #gen/choicemap {:x 0}))))

(defn iterable-seq [^Iterable iter]
  (when (.hasNext iter)
    (lazy-seq
     (cons (.next iter)
           (iterable-seq iter)))))

(deftest interface-tests
  (checking "Interface tests for choice maps"
            [m (gen-choicemap gen/keyword gen/any-equatable)]
            #?(:clj
               (is (= (seq m)
                      (iterable-seq
                       (.iterator ^Iterable m)))
                   "iterator impl matches seq"))

            (is (= m (choicemap/choicemap
                      (zipmap (keys m) (vals m))))
                "keys and vals work correctly")))

;; ## Trace Tests

(deftest binding-tests
  (letfn [(f [_] "hi!")]
    (binding [dynamic.trace/*trace* f]
      (is (= f (dynamic.trace/active-trace))
          "active-trace reflects dynamic bindings"))))

(defn choice-trace [x]
  (reify trace/ITrace
    (get-choices [_] (dynamic.choicemap/choice x))))

(deftest empty?
  (let [trace (dynamic.trace/trace (gen []) [])]
    (is (clojure/empty? trace))))

(deftest gf
  (let [gf (gen [])]
    (is (= gf (trace/get-gen-fn (dynamic.trace/trace gf []))))))

(deftest args
  (is (= [] (trace/get-args (dynamic.trace/trace (gen []) []))))
  (is (= [0] (trace/get-args (dynamic.trace/trace (gen [x] x) [0]))))
  (is (= [0 1] (trace/get-args (dynamic.trace/trace (gen [x y] (+ x y)) [0 1])))))

(deftest call-position
  (let [trace (dynamic.trace/trace (gen []) [])]
    (is (nil? (trace :addr))))
  (let [trace (-> (dynamic.trace/trace (gen []) [])
                  (dynamic.trace/assoc-subtrace :addr (choice-trace :x)))]
    (is (= :x (trace :addr)))))

(deftest keys
  (is (= {} (into {} (dynamic.trace/trace (gen []) [])))
      "iterator works on an empty trace")

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

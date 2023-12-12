(ns gen.distribution-test
  (:require [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [clojure.test :refer [is testing]]
            [clojure.test.check.generators :as gen]
            [gen.diff :as diff]
            [gen.distribution :as dist]
            [gen.dynamic.choice-map :as choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace]
            [gen.generators :refer [gen-double within]]
            [same.core :refer [ish? zeroish? with-comparator]]))

(defn gamma-tests [->gamma]
  (testing "spot checks"
    (is (= -6.391804444241573 (dist/logpdf (->gamma 0.001 1) 0.4)))
    (is (= -393.0922447210179 (dist/logpdf (->gamma 1 0.001) 0.4)))))

(defn student-t-tests [->student-t]
  (testing "spot checks"
    (with-comparator (within 1e-12)
      (is (ish? -1.7347417805005154 (dist/logpdf (->student-t 2 2.1 2) 2)))
      (is (ish? -2.795309741614719 (dist/logpdf (->student-t 1 0.8 4) 3)))))

  (checking "Student's T matches generalized logpdf"
            [v  (gen-double -10 10)
             nu (gen/fmap inc gen/nat)]
            (is (= (dist/logpdf (->student-t nu 0 1) v)
                   (dist/logpdf (->student-t nu) v))
                "these two paths should produce the same results")))

(defn beta-tests [->beta]
  (testing "spot checks"
    (is (= -5.992380837839856 (dist/logpdf (->beta 0.001 1) 0.4)))
    (is (= -6.397440480839912 (dist/logpdf (->beta 1 0.001) 0.4)))))

(defn bernoulli-tests [->bernoulli]
  (checking "Bernoulli properties"
            [p (gen-double 0 1)
             v gen/boolean]
            (is (= (dist/logpdf (->bernoulli 0.5) v)
                   (dist/logpdf (->bernoulli 0.5) (not v)))
                "Fair coin has equal chance")

            (is (ish? 1.0
                      (+ (Math/exp (dist/logpdf (->bernoulli p) v))
                         (Math/exp (dist/logpdf (->bernoulli p) (not v)))))
                "All options sum to 1")))

(defn categorical-tests [->cat]
  (checking "map => categorical properties"
            [p (gen-double 0 1)]
            (let [dist (->cat {:true p :false (- 1 p)})]
              (is (ish? (Math/log p) (dist/logpdf dist :true))
                  "prob of `:true` matches `p`")

              (is (ish? (Math/log (- 1 p)) (dist/logpdf dist :false))
                  "prob of `:false` matches `1-p`")))

  (checking "vector => categorical properties"
            [p (gen-double 0 1)]
            (let [dist (->cat [p (- 1 p)])]
              (is (ish? (Math/log p) (dist/logpdf dist 0))
                  "prob of `1` matches `p`")

              (is (ish? (Math/log (- 1 p)) (dist/logpdf dist 1))
                  "prob of `0` matches `1-p`"))))

(defn bernoulli-gfi-tests [bernoulli-dist]
  (testing "bernoulli-call-no-args"
    (is (boolean? (bernoulli-dist))))

  (testing "bernoulli-call-args"
    (is (boolean? (bernoulli-dist 0.5))))

  (testing "bernoulli-gf"
    (is (= bernoulli-dist (trace/get-gen-fn (gf/simulate bernoulli-dist [])))))

  (testing "bernoulli-args"
    (is (= [0.5] (trace/get-args (gf/simulate bernoulli-dist [0.5])))))

  (testing "bernoulli-retval"
    (is (boolean? (trace/get-retval (gf/simulate bernoulli-dist [0.5])))))

  (testing "bernoulli-choices-noargs"
    (is (boolean?
         (choice-map/unwrap
          (trace/get-choices (gf/simulate bernoulli-dist []))))))

  (testing "bernoulli-update-weight"
    (is (= 1.0
           (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
               (:trace)
               (trace/update #gen/choice true)
               (:weight)
               (Math/exp))))

    (is (= (/ 0.7 0.3)
           (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
               (:trace)
               (trace/update #gen/choice false)
               (:weight)
               (Math/exp)))))

  (testing "bernoulli-update-discard"
    (is (nil?
         (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
             (:trace)
             (trace/update nil)
             (:discard))))

    (is (= #gen/choice true
           (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
               (:trace)
               (trace/update #gen/choice false)
               (:discard)))))

  (testing "bernoulli-update-change"
    (is (= diff/unknown-change
           (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
               (:trace)
               (trace/update nil)
               (:change))))

    (is (= diff/unknown-change
           (-> (gf/generate bernoulli-dist [0.3] #gen/choice true)
               (:trace)
               (trace/update #gen/choice false)
               (:change))))))

(defn cauchy-tests [->cauchy]
  (checking "Cauchy properties"
            [scale (gen-double 0.001 100)
             v     (gen-double -100 100)]
            (is (= (dist/logpdf (->cauchy 0 scale) v)
                   (dist/logpdf (->cauchy 0 scale) (- v)))
                "symmetric about location"))

  (testing "spot checks"
    (is (= -1.1447298858494002 (dist/logpdf (->cauchy 1 1) 1)))
    (is (= -1.8378770664093453 (dist/logpdf (->cauchy 2 2) 2)))))

(defn delta-tests [->delta]
  (checking "Delta properties"
            [center (gen-double -100 100)
             v      (gen-double -100 100)]
            (if (= center v)
              (is (= 0.0    (dist/logpdf (->delta center) v)))
              (is (= ##-Inf (dist/logpdf (->delta center) v))))))

(defn exponential-tests [->exponential]
  (checking "exponential will never produce negative values"
            [v    (gen-double -100 -0.00001)]
            (is (= ##-Inf (dist/logpdf (->exponential 1.0) v))))

  (checking "rate 1.0 produces -v"
            [v (gen-double 0 100)]
            (is (= (- v) (dist/logpdf (->exponential 1.0) v))))

  (testing "spot checks"
    (is (= -3.3068528194400546 (dist/logpdf (->exponential 2.0) 2.0)))
    (is (= -5.306852819440055  (dist/logpdf (->exponential 2.0) 3.0)))))

(defn laplace-tests [->laplace]
  (checking "Laplace properties"
            [v (gen-double -10 10)]
            (let [log-l (dist/logpdf (->laplace 0 1) v)]
              (is (if (neg? v)
                    (is (= log-l (- v (Math/log 2))))
                    (is (= log-l (- (- v) (Math/log 2)))))
                  "location 0, scale 1"))

            (is (= (dist/logpdf (->laplace 0 1) v)
                   (dist/logpdf (->laplace 0 1) (- v)))
                "symmetric about location"))

  (checking "Laplace with scale 1, location == v"
            [v (gen-double -10 10)]
            (is (is (= (- (Math/log 2))
                       (dist/logpdf (->laplace v 1) v)))))

  (testing "spot checks"
    (is (= -1.6931471805599454 (dist/logpdf (->laplace 2 1) 1)))
    (is (= -1.8862943611198906 (dist/logpdf (->laplace 0 2) 1)))
    (is (= 4.214608098422191   (dist/logpdf (->laplace 0 0.001) 0.002)))))

(defn normal-tests [->normal]
  (checking "Normal properties"
            [mu    (gen-double -10 10)
             sigma (gen-double 0.001 10)
             v     (gen-double -100 100)
             shift (gen-double -10 10)]
            (is (ish? (dist/logpdf (->normal 0.0 sigma) v)
                      (dist/logpdf (->normal 0.0 sigma) (- v)))
                "Normal is symmetric about the mean")

            (with-comparator (within 1e-12)
              (is (ish? (dist/logpdf (->normal mu sigma) v)
                        (dist/logpdf (->normal (+ mu shift) sigma) (+ v shift)))
                  "shifting by the mean is a symmetry")))

  (testing "spot checks"
    (is (= -1.0439385332046727 (dist/logpdf (->normal 0 1) 0.5)))
    (is (= -1.643335713764618 (dist/logpdf (->normal 0 2) 0.5)))
    (is (= -1.612085713764618 (dist/logpdf (->normal 0 2) 0)))))

(defn uniform-tests [->uniform]
  (checking "(log of the) Beta function is symmetrical"
            [min (gen-double -10 0)
             max (gen-double 0 10)
             v   (gen-double -10 10)]
            (let [log-l (dist/logpdf (->uniform min max) v)]
              (if (<= min v max)
                (is (zeroish?
                     (+ log-l (Math/log (- max min))))
                    "Inside the bounds, log-l*range == 1.0")
                (is (= ##-Inf log-l)
                    "Outside the bounds, (log 0.0)")))))

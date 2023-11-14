(ns gen.diff-test
  "Tests for the [[gen.diff]] namespace."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.diff :as diff]))

(deftest diffed-tests
  (checking "strip-diff / get-diff"
            [x gen/any-equatable]
            (is (= x (diff/strip-diff x))
                "Non-wrapped values strip to themselves.")

            (is (= diff/no-change (diff/get-diff x))
                "Non-wrapped values report no change."))

  (checking "diff? is false for non-implementers"
            [x gen/any-equatable]
            (is (not (diff/diff? x)))))

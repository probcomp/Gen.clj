(ns gen.array-test
  "Tests for the [[gen.array]] namespace and the various base implementations
  that live there."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [gen.array :as array]))

(defn array-round-trip [m]
  (is (= m
         (->> (array/to-array m)
              (array/from-array m)))
      "entry round-trips through array."))

;; Wrapper for a non-collection, single element for testing.
(defrecord Element [v]
  array/IArray
  (to-array [_] [v])
  (-from-array [_ xs idx]
    [1 (Element. (nth xs idx))]))

(deftest from-array-test
  (checking "Element round-trips through array" 100
            [elem (gen/fmap ->Element gen/any-equatable)]
            (array-round-trip elem))

  (checking "failure on incorrect array source" 100
            [elem (gen/fmap ->Element gen/any-equatable)]
            (is (thrown?
                 #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (array/from-array elem [1 2 3]))
                "input array is too big!")))

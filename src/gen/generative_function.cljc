(ns gen.generative-function
  (:require [gen.trace :as trace]
            [gen.choice-map :as choice-map]))

;; https://www.gen.dev/docs/stable/ref/gfi/#Generative-function-interface-1

(defprotocol IGenerativeFunction
  (has-argument-grads [gf]
    "Return a tuple of booleans indicating whether a gradient is available for
    each of its arguments.")

  (accepts-output-grad? [gf]
    "Returns a boolean indicating whether the return value is dependent on any of
    the gradient source elements for any trace.")

  (get-params [gf]
    "Returns an iterable over the trainable parameters of the generative
    function.")

  (simulate [gf args]
    "Executes a generative function and return the trace."))

(defprotocol IGenerate
  (-generate [gf args constraints]
    "Returns a trace of a generative function that is consistent with the given
    constraints on the random choices."))

(defprotocol IPropose
  (propose [gf args]
    "Samples an assignment and compute the probability of proposing that
    assignment. Returns a triple of the form

```
(<choices> <weight> <retval>)
```"))

(defprotocol IAssess
  :extend-via-metadata true
  (assess [gf args choices]
    "Returns a pair of:

  - probability of proposing an assignment
  - the return value"))

;; ## API

(defn generate
  "Returns a trace of a generative function `gf` that is consistent with the given
  `constraints` on the random choices.

  If `constraints` aren't supplied, the implementation defaults to

  ```clojure
  (let [trace (simulate gf args)]
     {:trace trace
      :weight 0.0})
  ```"
  ([gf args]
   (let [trace (simulate gf args)]
     {:trace trace
      :weight 0.0}))
  ([gf args constraints]
   ;; TODO test constraints that are NOT a choice map and convert them on the
   ;; way in!
   ;;
   ;; TODO also guard against empty constraints.
   ;;
   ;; TODO test that all get_submap return empty submap on missing constraints.
   (let [constraints (if (choice-map/choice-map? constraints)
                       constraints
                       (choice-map/choicemap constraints))]
     (if (empty? constraints)
       (generate gf args)
       (-generate gf args constraints)))))

(defn ^:no-doc default-propose [gf args]
  (let [trace  (simulate gf args)
        weight (trace/get-score trace)]
    [(trace/get-choices trace)
     weight
     (trace/get-retval trace)]))

(extend-protocol IPropose
  #?(:clj Object :cljs default)
  (propose [gf args] (default-propose gf args)))

(defn ^:no-doc default-assess [gf args choices]
  (let [{:keys [trace weight]} (-generate gf args choices)]
    [weight (trace/get-retval trace)]))

(extend-protocol IAssess
  #?(:clj Object :cljs default)
  (assess [gf args choices] (default-assess gf args choices)))

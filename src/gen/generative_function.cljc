(ns gen.generative-function
  "This namespace declares the core abstractions of Gen.clj. Along
  with [[gen.trace]] and [[gen.choicemap]], these protocols make up the core of
  the language."
  (:require [gen.trace :as trace]
            [gen.choicemap :as choicemap]))

;; ## The GFI
;;
;; The generative function interface of [[IGenerativeFunction]] is the key
;; addition of Gen.clj to the Clojure language. Probabilistic programs written
;; in Gen.clj involve calls to "generative functions", or instances of this
;; protocol, that internally sample from other generative functions using
;; labeled addresses.
;;
;; These executions are recorded in instances of [[gen.trace/ITrace]] and the
;; random choices are exposed for navigation in instances
;; of [[gen.choicemap/IChoiceMap]].
;;
;; For the clearest implementation of [[IGenerativeFunction]] and its associated
;; trace and choicemap types, see [[gen.dynamic]].

(defprotocol IGenerativeFunction
  (has-argument-grads [gf]
    "Return a sequence of booleans indicating whether a gradient is available
    for each of its arguments.")

  (accepts-output-grad? [gf]
    "Returns a boolean indicating whether the return value is dependent on any of
    the gradient source elements for any trace.")

  (get-params [gf]
    "Returns a sequence of trainable parameters of the generative function.")

  (simulate [gf args]
    "Executes a generative function `gf` using the supplied `args` and returns
    an [[gen.trace/ITrace]] instance with the results of the execution."))

;; ## IGenerate

(defprotocol IGenerate
  (-generate [gf args constraints]
    "Generates a trace of the supplied generative function `gf` called with
    `args` and consistent with the supplied `constraints`, and returns a map of
    the form:

    ```clojure
    {:trace  <trace>
     :weight <weight>}
    ```"))

;; ## IPropose
;;
;; Types can specialize their `propose` implementations with this protocol.
;; Without that specialization, they fall back to [[default-propose]], whch is
;; built on top of [[simulate]].

(defprotocol IPropose
  (propose [gf args]
    "Samples an assignment and compute the probability of proposing that
    assignment. Returns a map of the form

    ```clojure
    {:choices <choices>
     :weight <retval>
     :retval <retval>}
    ```"))

;; ## IAssess

;; Similarly, types can specialize their `-assess` implementations with this
;; protocol. Without that specialization, they fall back to [[default-assess]],
;; whch is built on top of [[generate]].

(defprotocol IAssess
  (-assess [gf args choices]
    "Returns a map of the form:

    ```clojure
    {:weight <probability of proposing the `choices` assignment>
     :retval <retval of the `gf` applied to `args`>}
    ```"))

;; ## API
;;
;; This version of [[generate]] provides small affordances over the protocol [[-generate]] (note the dash).
;;
;; - a 2-arity version built on [[simulate]] works for all callers
;; - Non-choicemap constraints are converted to constraints on the way in
;; - empty constraints fall back on the 2-arity implementation.

(defn generate
  "Returns a trace of a generative function `gf` that is consistent with the given
  `constraints` on the random choices.

  `constraints` should be a [[gen.choicemap/IChoiceMap]] instance. Any other
  type will be converted to a choicemap via [[gen.choicemap/choicemap]].

  If `constraints` aren't supplied (or if the supplied constraints are empty),
  the implementation defaults to

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
   (let [constraints (choicemap/choicemap constraints)]
     (-generate gf args constraints))))

(defn ^:no-doc default-propose
  "Default implementation of [[propose]], built on [[simulate]]."
  [gf args]
  (let [trace  (simulate gf args)
        weight (trace/get-score trace)]
    [(trace/get-choices trace)
     weight
     (trace/get-retval trace)]))

(extend-protocol IPropose
  #?(:clj Object :cljs default)
  (propose [gf args] (default-propose gf args)))

(defn ^:no-doc default-assess
  "Default implementation of [[-assess]], built on [[generate]]."
  [gf args choices]
  (let [{:keys [trace weight]} (generate gf args choices)]
    [weight (trace/get-retval trace)]))

(extend-protocol IAssess
  #?(:clj Object :cljs default)
  (-assess [gf args choices] (default-assess gf args choices)))

(defn assess
  "Returns a map of the form:

  ```clojure
  {:weight <probability of proposing the `choices` assignment>
   :retval <retval of the `gf` applied to `args`>}
  ```"
  [gf args choices]
  (-assess gf args (choicemap/choicemap choices)))

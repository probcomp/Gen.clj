(ns gen.trace
  "Protocols that constitute the trace interface."
  (:refer-clojure :exclude [update])
  (:require [gen.diff :as diff]))

;; https://github.com/probcomp/Gen.jl/blob/master/src/gen_fn_interface.jl#L1

(defprotocol ITrace
  (get-args [trace] "Returns the argument tuple for a given execution.")
  (get-retval [trace] "Returns the return value of a given execution.")
  (get-choices [trace]
    "Returns an instance of [[gen.choice-map/IChoiceMap]].

  NOTE that the value of any non-addressed randomness is not externally
  accessible.")

  (get-gen-fn [trace]
    "Returns the generative function that produced the given `trace`.")

  (get-score [trace] "Returns

 $$\\log \\frac{p(r, t; x)}{q(r; x, t)}$$

 When there is no non-addressed randomness, this simplifies to the log
 probability $\\log p(t; x)$."))

(defprotocol IProject
  (project [trace selection]
    "Estimates the probability that the selected choices take the values they do
    in a trace."))

;; From the original docs: "If you want to use MCMC on models that call your
;; generative function, then implement [[IUpdate]] and [[IRegenerate]]."

(defprotocol IUpdate
  (-update [trace args argdiffs constraints]
    "Updates a trace by changing the arguments and / or providing new values for
    some existing random choice(s) and values for some newly introduced random
    choice(s)."))

(defprotocol IRegenerate
  (-regenerate [trace args argdiffs selection]
    "Updates a trace by changing the arguments and / or randomly sampling new
    values for selected random choices using the internal proposal distribution
    family."))

(defprotocol ITrainable
  (accumulate-param-gradients! [_ _ _]
    "Increment gradient accumulators for parameters by the gradient of the
  log-probability of the trace, optionally scaled, and return the gradient with
  respect to the arguments (not scaled).

NOTE that the original version assumed mutable traces and we probably want to
back this out."))

(defprotocol IChoiceGradients
  (choice-gradients [trace selection retgrad]
    "Returns a triple of the form

```
[<arg-grads>, <choice-values>, <choice-grads>]
```"))

;; ## API

(defn trace? [t]
  (satisfies? ITrace t))

(defn can-project? [t]
  (instance? IProject t))

(defn can-update? [t]
  (satisfies? IUpdate t))

(defn can-regenerate? [t]
  (satisfies? IRegenerate t))

(defn update
  "Updates a trace by changing the arguments and / or providing new values for
    some existing random choice(s) and values for some newly introduced random
   choice(s).

  The 2-arity version is a shorthand variant which assumes the arguments are
  unchanged."
  ([trace constraints]
   (let [args  (get-args trace)
         diffs (repeat (count args) diff/no-change)]
     (-update trace args diffs constraints)))
  ([trace args argdiffs constraints]
   (-update trace args argdiffs constraints)))

(defn regenerate
  "Updates a trace by changing the arguments and / or randomly sampling new
    values for selected random choices using the internal proposal distribution
    family.

  The 2-arity version is a shorthand variant which assumes the arguments are
  unchanged."
  ([trace selection]
   (let [args  (get-args trace)
         diffs (repeat (count args) diff/no-change)]
     (-update trace args diffs selection)))
  ([trace args argdiffs selection]
   (-update trace args argdiffs selection)))

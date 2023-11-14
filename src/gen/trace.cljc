(ns gen.trace
  "Defines the [[ITrace]] abstraction and its API."
  (:refer-clojure :exclude [update])
  (:require [gen.choicemap :as choicemap]
            [gen.diff :as diff]))

;; ## ITrace
;;
;; The generative function interface (see [[gen.generative-function]]) acts on
;; generative functions to produce [[ITrace]] instances, or "traces". A trace is
;; a record of the execution of a given generative function and all of the
;; addressed random choices made during that execution.
;;
;; The [[ITrace]] protocol defines the basic contract that all trace
;; implementations must meet.
;;
;; For a straightforward example implementation, see [[gen.dynamic/Trace]] and
;; its accompanying [[gen.dynamic/ChoiceMap]].

(defprotocol ITrace
  (get-args [trace] "Returns the argument tuple for the execution that generated
  the supplied `trace`.")
  (get-retval [trace] "Returns the return value of the execution that generated
  the supplied `trace`.")
  (get-choices [trace]
    "Returns an instance of [[gen.choicemap/IChoiceMap]] that provides access to
    the choices made during the course of the execution that generated the
    supplied `trace`.

    NOTE that the value of any non-addressed randomness is not externally
    accessible.")

  (get-gen-fn [trace]
    "Returns the generative function whose execution produced the given
    `trace`.")

  (get-score [trace] "Returns

 $$\\log \\frac{p(r, t; x)}{q(r; x, t)}$$

 When there is no non-addressed randomness, this simplifies to the log
 probability $\\log p(t; x)$."))

;; ## IUpdate

;; (From the original docs: "If you want to use MCMC on models that call your
;; generative function, then implement [[IUpdate]].)
;;
;; [[-update]] update a trace by changing the arguments and/or providing new
;; values for some existing random choice(s) and values for some newly
;; introduced random choice(s).

;; Given

;; - a previous trace $(x, t, r)$ (`trace`)
;; - new arguments $x'$ (`args`), and
;; - a map $u$ (`constraints`),
;;
;; return a new trace $(x', t', r')$ (`new_trace`) that is consistent with
;; $u$.
;;
;; The values of choices in $t'$ are either copied from $t$ or from
;; $u$ (with $u$ taking precedence) or are sampled from the internal
;; proposal distribution. All choices in $u$ must appear in $t'$.
;;
;; Also return an assignment $v$ (`discard`) containing the choices in $t$ that
;; were overwritten by values from $u$, and any choices in $t$ whose address
;; does not appear in $t'$. Sample $t' \\sim q(\\cdot; x', t + u)$, and $r'
;; \\sim q(\\cdot; x', t')$, where $t + u$ is the choice map obtained by merging
;; $t$ and $u$ with $u$ taking precedence for overlapping addresses.
;;
;; Also return a weight (`weight`): ```math \\log \\frac{p(r', t'; x') q(r; x,
;; t)}{p(r, t; x) q(r'; x', t') q(t'; x', t + u)} ```
;;
;; Note that `argdiffs` is expected to be the same length as `args`. If the
;; function that generated `trace` supports default values for trailing
;; arguments, then these arguments can be omitted from `args` and `argdiffs`.
;; Note that if the original `trace` was generated using non-default argument
;; values, then for each optional argument that is omitted, the old value will
;; be over-written by the default argument value in the updated trace.

(defprotocol IUpdate
  (-update [trace args argdiffs constraints]
    "Generates an updated trace by

     - changing the arguments that generated the given `trace`, and / or
     - and / or providing
       - new values for some existing random choice(s), and
       - values for some newly introduced random choice(s).

     and returns a map of the form

    ```clojure
    {:trace   <new-trace>
     :change  <[[gen.diff/IDiff]] instance>
     :weight  <new-weight>
     :discard <choices in `trace` that were overwritten>}
    ```

    The new weight's value is the following:

    $$\\log \\frac{p(r', t'; x') q(r; x, t)}{p(r, t; x) q(r'; x', t') q(t'; x', t + u)}$$"))

;; ## IProject

(defprotocol IProject
  (project [trace selection]
    "Estimates the probability that choices specified by `selection` take the
    values they do in the supplied `trace`.

    Given a trace $(x, t, r)$ (`trace`) and a set of addresses
    $A$ (`selection`), let $u$ denote the restriction of $t$ to $A$.

    Returns the `weight`:

    $$\\log \\frac{p(r, t; x)}{q(t; u, x) q(r; x, t)}$$"))

;; ## ITrainable

;; Given a previous trace $(x, t)$ (`trace`) and a gradient with respect to the
;; return value $∇_y J$ (`retgrad`), return the following gradient (`arg_grads`)
;; with respect to the arguments $x$:
;;
;; $$∇_x \\left( \\log P(t; x) + J \\right)$$
;;
;; The length of `arg_grads` will be equal to the number of arguments to the
;; function that generated `trace` (including any optional trailing arguments).
;; If an argument is not annotated with `(grad)`, the corresponding value in
;; `arg_grads` will be `nothing`.
;;
;; Also increment the gradient accumulators for the trainable parameters $Θ$ of
;; the function by:
;;
;; $$∇_Θ \\left( \\log P(t; x) + J \\right)$$

(defprotocol ITrainable
  (accumulate-param-gradients! [_ _ _]
    "Increment gradient accumulators for parameters by the gradient of the
  log-probability of the trace, optionally scaled, and return the gradient with
  respect to the arguments (not scaled).

  NOTE that the original version assumed mutable traces; we probably will not
  support this, and will rename the method to have no `!` to reflect this
  change."))

;; ## IChoiceGradients

;; Given a previous trace $(x, t)$ (`trace`) and a gradient with respect to the
;; return value $∇_y J$ (`retgrad`), return the following gradient (`arg_grads`)
;; with respect to the arguments $x$:
;;
;; $$∇_x \\left( \\log P(t; x) + J \\right)$$
;;
;; The length of `arg_grads` will be equal to the number of arguments to the
;; function that generated `trace` (including any optional trailing arguments).
;; If an argument is not annotated with `(grad)`, the corresponding value in
;; `arg_grads` will be `nothing`.
;;
;; Also given a set of addresses $A$ (`selection`) that are continuous-valued
;; random choices, return the folowing gradient (`choice_grads`) with respect to
;; the values of these choices:

;; $$∇_A \\left( \\log P(t; x) + J \\right)$$

;; The gradient is represented as a choicemap whose value at (hierarchical)
;; address `addr` is $∂J/∂t[\\texttt{addr}]$.

;; Also return the choicemap (`choice_values`) that is the restriction of $t$ to
;; $A$.

(defprotocol IChoiceGradients
  (choice-gradients [trace selection retgrad]
    "Returns a map of the form

    ```clojure
    {:arg-grads     <>
     :choice-values <>
     :choice-grads  <>}
    ```"))

;; ## API

(defn trace?
  "Returns true if `t` satisfies the [[ITrace]] protocol, false otherwise."
  [t]
  (satisfies? ITrace t))

(defn can-project?
  "Returns true if `t` satisfies the [[IProject]] protocol, false otherwise."
  [t]
  (instance? IProject t))

(defn can-update?
  "Returns true if `t` satisfies the [[IUpdate]] protocol, false otherwise."
  [t]
  (satisfies? IUpdate t))

(defn update
  "Generates an updated trace by

   - changing the arguments that generated the given `trace`, and / or
   - and / or providing
     - new values for some existing random choice(s), and
     - values for some newly introduced random choice(s).

   and returns a map of the form

  ```clojure
  {:trace   <new-trace>
   :change  <[[gen.diff/IDiff]] instance>
   :weight  <new-weight>
   :discard <choices in `trace` that were overwritten>}
  ```

  The new weight's value is the following:

  $$\\log \\frac{p(r', t'; x') q(r; x, t)}{p(r, t; x) q(r'; x', t') q(t'; x', t + u)}$$

  The 2-arity version is a shorthand variant which assumes the arguments are
  unchanged."
  ([trace constraints]
   (let [args        (get-args trace)
         diffs       (repeat (count args) diff/no-change)
         constraints (choicemap/choicemap constraints)]
     (-update trace args diffs constraints)))
  ([trace args argdiffs constraints]
   (let [constraints (choicemap/choicemap constraints)]
     (-update trace args argdiffs constraints))))

(defn trace->map
  "Given an [[ITrace]] instance `t`, returns a map representation of `t` of the form

  ```clojure
  {:gen-fn  (get-gen-fn t)
   :args    (get-args t)
   :retval  (get-retval t)
   :choices (get-choices t)
   :score   (get-score t)}
  ```

  Useful for printing and inspecting arbitrary [[ITrace]] instances."
  [t]
  {:gen-fn  (get-gen-fn t)
   :args    (get-args t)
   :retval  (get-retval t)
   :choices (get-choices t)
   :score   (get-score t)})

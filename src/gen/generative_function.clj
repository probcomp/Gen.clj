(ns gen.generative-function)

;; https://www.gen.dev/docs/stable/ref/gfi/#Generative-function-interface-1

(defprotocol Simulate
  :extend-via-metadata true
  (simulate [gf args]
    "Executes a generative function and return the trace."))

(defprotocol Generate
  :extend-via-metadata true
  (generate
    [gf args]
    [gf args constraints]
    "Returns a map with keys `:trace`, `:weight` and, optionally, `:discard`...
    of a generative function that is consistent with the given constraints on
    the random choices."))

#_
(defprotocol Propose
  :extend-via-metadata true
  (propose [gf args]
    "Samples an assignment and compute the probability of proposing that
    assignment."))

#_
(defprotocol Assess
  :extend-via-metadata true
  (assess [gf args choices]
    "Returns the probability of proposing an assignment"))

#_
(defprotocol HasArgumentGrads
  :extend-via-metadata true
  (has-argument-grads [gf]))

#_
(defprotocol AcceptsOutputGrad
  :extend-via-metadata true
  (accepts-output-grad? [gf]
    "Returns a boolean indicating whether the return value is dependent on any of
    the gradient source elements for any trace."))

#_
(defprotocol Params
  :extend-via-metadata true
  (params [gf]
    "Returns an iterable over the trainable parameters of the generative
    function."))

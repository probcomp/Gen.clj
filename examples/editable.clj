^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns editable
  "This is a slimmed-down version of `intro-to-modeling`. The editor view doesn't
  handle caching well, so I've removed the more expensive calls at the end of
  `intro-to-modeling`, and also changed the `pmap` call in `prepeatedly` into
  `map`, since we don't have `pmap` available in the browser."
  {:nextjournal.clerk/toc true}
  (:require [gen.choicemap :refer [choicemap]]
            [gen.clerk.callout :as callout]
            [gen.clerk.viewer :as viewer]
            [gen.distribution.kixi :as dist]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.inference.importance :as importance]
            [gen.trace :as trace]
            [nextjournal.clerk :as clerk]))

;; # Tutorial: Introduction to modeling in Gen.clj

;; Welcome! In this tutorial, you'll get your feet wet with Gen.clj. Gen.clj is
;; a Clojure implementation of Gen, a multi-paradigm platform for probabilistic
;; modeling and inference. By "multi-paradigm," we mean that Gen supports many
;; different approaches to modeling and inference:

;; - Unsupervised learning and posterior inference in generative models using
;;   Monte Carlo, variational, EM, and stochastic gradient
;;   techniques.

;; - Supervised learning of conditional inference models (e.g. supervised
;;   classification and regression).

;; - Hybrid approaches including amortized inference / inference compilation,
;;   variational autoencoders, and semi-supervised learning.

;; Don't worry if you haven't seen some of these approaches before. One goal of
;; these tutorials will be to introduce you to a subset of them, from a unified
;; probabilistic programming perspective.

^{::clerk/visibility {:code :hide}}
(callout/alert
 :title
 "\"Gen\" vs \"Gen.clj\""
 :message
 "Gen.clj, the Clojure implementation of the Gen language, currently only
 supports a subset of Gen's features. For a complete implementation see
 [Gen.jl](https://github.com/probcomp/Gen.jl). If you would like to get involved
 with Gen.clj's development please [contact us](mailto:contributing@zane.io).")

;; ### In this Tutorial

;; Approaching a problem from a probabilistic perspective requires both
;; *modeling* and *inference*:

;; - **Modeling**: You first need to frame the problem — and any assumptions you
;;   bring to the table — as a probabilistic model. A huge variety of problems
;;   can be viewed from a modeling & inference lens, if you set them up
;;   properly.  **This notebook is about how to think of problems in this light,
;;   and how to use Gen** **to formally specify your assumptions and the tasks
;;   you wish to solve.**

;; - **Inference**: You then need to do the hard part: inference, that is,
;;   solving the problem. In this notebook, we'll use a particularly simple
;;   *generic* inference algorithm: importance sampling with the prior as our
;;   proposal distributions. With enough computation, the algorithm can in
;;   theory solve any modeling and inference problem, but in practice, for most
;;   problems of interest, it is too slow to achieve accurate results in a
;;   reasonable amount of time.  **Future tutorials introduce some of Gen's**
;;   **programmable inference features**, which let you tailor the inference
;;   algorithm for use with more complex models (Gen will still automate the
;;   math!).

;; Throughout this tutorial, we will emphasize key degrees of modeling
;; flexibility afforded by the probabilistic programming approach, for example:

;; - Using a stochastic branching and function abstraction to express
;;   uncertainty about which of multiple models is appropriate.

;; - Representing models with an unbounded number of parameters (a 'Bayesian
;;   non-parametric' model) using loops and recursion.

;; We'll also introduce a technique for validating a model and inference
;; algorithm by predicting new data from inferred parameters, and comparing this
;; data to the observed data set.

;; However, this tutorial does not exhaustively cover all features of Gen's
;; modeling language. For example, Gen's modeling combinators and its static
;; modeling language enable improved performance, but are not covered here.

;; ## 1. Clojure, Gen, and this Clerk notebook

;; Gen is a library for the Clojure programming language. The library can be
;; required with:
;;
;; ```clojure
;; (require '[gen.dynamic :as dynamic :refer [gen]])
;; ```

;; Gen programs typically consist of a combination of (i) probabilistic models
;; written in modeling languages and (ii) inference programs written in regular
;; Clojure code. Gen provides a built-in modeling language that is itself based
;; on Clojure.

;; This tutorial uses a Clerk notebook. All forms in the notebook are regular
;; Clojure expressions. We will use Clerk metadata to some expressions so that
;; the value of the expression is not printed.

(def a (+ 1 1))

^{::clerk/visibility {:result :hide}}
(def b (+ 1 1))

;; This notebook uses [vega-lite](https://vega.github.io/vega-lite/) for
;; plotting.

;; This notebook will make use of Clojure keywords. Note that a Clojure keyword
;; is different from a Clojure string:

(type :foo)

(type "foo")

;; Some expressions in this notebook will take a long time to evaluate. As such
;; we've wrapped those expressions in `clojure.core/delay`. You can force
;; evaluation of the delay and see the result by clicking on the 'force' button.
;; Try forcing the evaluation of the expression below now.

^{::clerk/viewer viewer/delay ::clerk/width :wide}
(def delay-example
  (delay (viewer/sleep 5000)
         :done))

;; ## 2. Writing a probabilistic model as a generative function

;; Probabilistic models are represented in Gen as *generative functions*.
;; Generative functions are used to represent a variety of different types of
;; probabilistic computations including generative models, inference models,
;; custom proposal distributions, and variational approximations (see the [Gen
;; documentation](https://probcomp.github.io/Gen/dev/ref/gfi/) or the
;; [paper](https://dl.acm.org/doi/10.1145/3314221.3314642)). In this
;; tutorial,
;; we focus on implementing _generative models_. A generative model represents
;; a data-generating process; as such, it encodes any assumptions we have about
;; our data and our problem domain.

;; The simplest way to construct a generative function is by using the built-in
;; modeling DSL. Generative functions written in the built-in modeling DSL are
;; based on Clojure function definition syntax, but use the `gen.dynamic/gen`
;; macro:

;; ``` clojure
;; (def function-name-here
;;   (gen [input-arguments]
;;     ;; function body
;;     ))
;; ```
;; The function represents the data-generating process we are modeling.
;; Conceptually, every time we run the function, it should generate a new
;; "synthetic dataset" in line with our assumptions. Along the way, it will make
;; random choices; each random choice it makes can be thought of as adding
;; a random variable to a probabilistic model.

;; Within the function body, most Clojure code is permitted, but random choices use
;; special syntax that annotates them with an _address_:

;; ``` clojure
;; (dynamic/trace! addr distribution parameters)
;; ```

;; A simple example of such an invocation is a normal distribution parametrized
;; with mean 0 and standard deviation 1:

;; ```clojure
;; (require '[gen.distribution.commons-math :as dist])
;; ```

(comment
  (def my-variable (dynamic/trace! :my-variable-address dist/normal 0 1)))

;; Every random choice must be given an _address_, which can be an arbitrary
;; value—but we often use a keyword.  (`:my-variable-address` is a keyword in
;; the Clojure language.)  Think of the address as the name of a particular
;; random choice, which is distinct from the name of the variable. For example,
;; consider the following code:

(comment
  (let [x (dynamic/trace! :initial-x dist/normal 0 1)]
    (if (< x 0)
      (+ x (dynamic/trace! :addition-to-x dist/normal 2 1))
      x)))

;; This code manipulates a single variable, `x`, but may make up to two random
;; choices: `:initial-x` and `:addition-to-x`.

;; Note that we can only use `dynamic/trace!` to give addresses to _random
;; choices_. The following will _not_ work because the code is trying to trace
;; the expression `sin(x)` which is an invocation of an ordinary Clojure
;; function, not a distribution.

;; ```Clojure
;; # INVALID:
;; (def my-variable (dynamic/trace! :not-a-random-choice clojure.math/sin x))
;; ```

;; (We will see a bit later that it is _also_ possible to use dynamic/trace! to
;; sample from helper _generative functions_, not just primitive distributions
;; like `normal`. But for now, think of `dynamic/trace!` as being for making
;; random choices.)

;; ### Example: Bayesian linear regression

;; Suppose we have a dataset of points $(x, y)$ in the plane, and we'd like to
;; infer a likely slope and intercept that explains their (linear) relationship.
;; To approach this problem from a probabilistic perspective, we first need to
;; develop a model. The model answers the question: how might this dataset have
;; come to be? It also encodes our assumptions, e.g., our assumption that our
;; data is explained by a linear relationship between $x$ and $y$.

;; The generative function below represents a probabilistic model of a linear
;; relationship in the x-y plane. Given a set of $x$ coordinates, it randomly
;; chooses a line in the plane and generates corresponding $y$ coordinates so
;; that each $(x, y)$ is near the line. We might think of this function as
;; modeling house prices as a function of square footage, or the measured volume
;; of a gas as a function of its measured temperature.

^{::clerk/visibility {:result :hide}}
(def line-model
  (gen [xs]

    ;; We begin by sampling a slope and intercept for the line.  Before we have
    ;; seen the data, we don't know the values of these parameters, so we treat
    ;; them as random choices. The distributions they are drawn from represent our
    ;; prior beliefs about the parameters: in this case, that neither the slope
    ;; nor the intercept will be more than a couple points away from 0.

    (let [slope (dynamic/trace! :slope dist/normal 0 1)
          intercept (dynamic/trace! :intercept dist/normal 0 2)

          ;; We define a function to compute y for a given x.

          y (fn [x]
              (+ (* slope x)
                 intercept))]

      ;; Given the slope and intercept, we can sample y coordinates for each of
      ;; the x coordinates in our input vector.

      (doseq [[i x] (map vector (range) xs)]
        (dynamic/trace! [:y i] dist/normal (y x) 0.1))

      ;; Most of the time, we don't care about the return
      ;; value of a model, only the random choices it makes.
      ;; It can sometimems be useful to return something
      ;; meaningful, however; here, we return the function `y`.
      y)))

;; The generative function takes as an argument a vector of x-coordinates. We
;; create one below:

(def xs (range -5 6))

;; Given this sequence, the generative function samples a random choice
;; representing the slope of a line from a normal distribution with mean `0` and
;; standard deviation `1`, and a random choice representing the intercept of a
;; line from a normal distribution with mean 0 and standard deviation `2`. In
;; Bayesian statistics terms, these distributions are the *prior distributions*
;; of the slope and intercept respectively. Then, the function samples values
;; for the y-coordinates corresponding to each of the provided x-coordinates.

;; This generative function returns a function `y` encoding the slope
;; and intercept.

;; We can run the model like we run a regular Clojure function:

(def y (line-model xs))

;; This gives us the return value of the model, but we may be more interested in
;; _the values of the random choices_ that `line_model` makes. **Crucially, each
;; random choice is annotated with a unique *address*.** A random choice is
;; assigned an address using the `(dynamic/trace! addr ...)` form. Addresses can be
;; any Clojure value.  In this program, there are two types of addresses used --
;; Clojure keywords and vectors of keywords and integers. Note that within the
;; `map-indexed` loop, the same line of code is executed multiple times, but
;; each time, the random choice it makes is given a distinct address.

;; Although the random choices are not included in the return value, they *are*
;; included in the *execution trace* of the generative function. We can run the
;; generative function and obtain its trace using the [`
;; simulate`](https://probcomp.github.io/Gen/dev/ref/gfi/#Gen.simulate) method
;; from the Gen API:
;;
;; ```clojure
;; (require '[gen.generative-function :as gf])
;; ```

(def trace (gf/simulate line-model [xs]))

;; This method takes the function to be executed, and a tuple of arguments to
;; the function, and returns a trace and a second value that we will not be
;; using in this tutorial.

;; A trace of a generative function contains various information about an
;; execution of the function. For example, it contains the arguments on which
;; the function was run, which are available with the API method
;; `gen.trace/get-args`:
;;
;; ```clojure
;; (require '[gen.trace :as trace])
;; ```


(trace/get-args trace)

;; The trace also contains the value of the random choices, stored in a map from
;; address to value called a *choice map*. This map is available through the API
;; method `gen.trace/get-choices`:

(trace/get-choices trace)

;; We can pull out individual values from this map using `clojure.core/get`:

(get (trace/get-choices trace) :slope)

;; Or we can use either the choice map as a function:

(let [choices (trace/get-choices trace)]
  (choices :slope))

;; Or we can call the keyword on the choice map:

(:slope (trace/get-choices trace))

;; We can also read the value of a random choice directly from the trace,
;; without having to use `gen.trace/get-choices` first:

(get trace :slope)

(trace :slope)

(:slope trace)

;; The return value is also recorded in the trace, and is accessible with the
;; `trace/get-retval` API method:

(trace/get-retval trace)

;; In order to understand the probabilistic behavior of a generative function,
;; it is helpful to be able to visualize its traces. Below, we define a Clerk
;; viewer to render a trace of the generative function above. The rendering
;; shows the x-y data points and the line that is represented by the slope and
;; intercept choices.

{::clerk/visibility {:result :hide}}

(defn render-trace-spec
  [trace & {:keys [clip x-domain y-domain] :or {clip false}}]
  (let [[xs] (trace/get-args trace) ; Pull out the xs from the trace.
        y (trace/get-retval trace) ; Pull out the return value, useful for plotting.
        ys (for [i (range (count xs))]
             (trace [:y i]))
        data (mapv (fn [x y]
                     {:x x :y y})
                   xs
                   ys)]
    {:schema "https://vega.github.io/schema/vega-lite/v5.json"
     :embed/opts {:actions false}
     :data nil
     :layer [{:data {:values (for [x (range -5 5 (/ 10 1000))]
                               {:x x
                                :y (y x)})}
              :mark {:type "line" :color "red" :clip clip}
              :encoding {:x {:field :x :type "quantitative" :scale {:domain x-domain}}
                         :y {:field :y :type "quantitative" :scale {:domain y-domain}}}}
             {:data {:values data}
              :mark {:type "circle" :clip clip}
              :encoding {:x {:field :x :type "quantitative"}
                         :y {:field :y :type "quantitative"}}}]}))

(def render-trace (comp clerk/vl render-trace-spec))

^{::clerk/visibility {:result :show}}
(render-trace trace)

;; Because a generative function is stochastic, we need to visualize many runs
;; in order to understand its behavior.
;;
;; The function below renders a grid of traces.

(defn grid
  ([renderer traces]
   (grid {} renderer traces))
  ([{:keys [width] :or {width 4}} renderer traces]
   (apply clerk/col
          {::clerk/width :full}
          (map #(apply clerk/row %)
               (partition-all width (map renderer traces))))))

(def traces (repeatedly 12 #(gf/simulate line-model [xs])))

{::clerk/visibility {:result :show}}

(grid #(render-trace % {:x-domain [-5 5]
                        :y-domain [-5 5]
                        :clip true})
      traces)

;; ### 2.1 Exercise

;; Write a generative function that uses the same address twice. Run it to see
;; what happens.

;; ### 2.2 Exercise

;; Write a model that generates a sine wave with random phase, period and
;; amplitude, and then generates y-coordinates from a given vector of
;; x-coordinates by adding noise to the value of the wave at each x-coordinate.
;; Use a `gamma(1, 1)` prior distribution for the period, and a `gamma(1, 1)`
;; prior distribution on the amplitude (see
;; `gen.distribution.commons-math/gamma`). Sampling from a Gamma distribution
;; will ensure to give us postive real values. Use a uniform distribution
;; between 0 and $2\pi$ for the phase (see
;; `gen.distribution.commons-math/uniform`).

;; The sine wave should implement:

;; $$ y(x) = a \sin(2\pi \frac{1}{p} x + \varphi) $$

;; where $a$ is the amplitude, $p$ is the period and $\varphi$ is the phase.  In
;; Clojure the constant $\pi$ can be expressed as `clojure.math/PI`.

^{::clerk/visibility {:result :hide}}
(require '[clojure.math :as math])
math/PI

;; When calling `(trace/get-choices (gf/simulate sine-model [xs]))`, the following
;; choices should appear:

;; - amplitude: `(trace :amplitude)`
;; - period: `(trace :period)`
;; - phase: `(trace :phase)`

;; We have provided some starter code for the sine wave model:

{::clerk/visibility {:result :hide}}

(def sine-model
  (gen [xs]

    ;; < your code here, for sampling a phase, period, and amplitude >

    (let [y (fn [_x]
              1)] ; < Edit this function to compute y for a given x >

      (dotimes [i (count xs)]
        (let [x (nth xs i)]
          (dynamic/trace! [:y i] dist/normal (y x) 0.1)))

      y))) ; We return the `y` function so it can be used for plotting, below.

(def sine-model-traces (repeatedly 12 #(gf/simulate sine-model [xs])))

(defn render-sine-model-trace
  [trace]
  (let [[xs] (trace/get-args trace) ; Pull out the xs from the trace.
        min-x (apply min xs)
        max-x (apply max xs)
        y (trace/get-retval trace) ; Pull out the return value, useful for plotting.
        ys (for [i (range (count xs))]
             (get trace [:y i]))
        data (mapv (fn [x y]
                     {:x x :y y})
                   xs
                   ys)]
    (clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
               :embed/opts {:actions false}
               :layer [{:data {:values (for [x (range min-x max-x (/ (- max-x min-x)
                                                                     1000))]
                                         {:x x :y (y x)})}
                        :mark {:type "line" :color "red"}
                        :encoding {:x {:field :x :type "quantitative"}
                                   :y {:field :y :type "quantitative"}}}
                       {:data {:values data}
                        :mark "circle"
                        :encoding {:x {:field :x :type "quantitative"}
                                   :y {:field :y :type "quantitative"}}}]})))

{::clerk/visibility {:result :show}}

(grid render-sine-model-trace sine-model-traces)

;; **Solution**

^{::clerk/visibility {:code :fold :result :hide}}
(comment
  (def sine-model
    (gen [xs]
      (let [period (dynamic/trace! :period dist/gamma 1 1)
            amplitude (dynamic/trace! :amplitude dist/gamma 1 1)
            phase (dynamic/trace! :phase dist/uniform 0 (* 2 math/PI))

            ;; Define a deteriministic sine wave with the values above.
            y (fn  [x]
                (* amplitude
                   (math/sin (+ (* x
                                   (/ (* 2 math/PI)
                                      period))
                                phase))))]

        (dotimes [i (count xs)]
          (let [x (nth xs i)]
            (dynamic/trace! [:y i] dist/normal (y x) 0.1)))

        y))))

;; ## 3. Doing Posterior inference

;; Of course, we don't really care about generating lots of pictures of lines
;; (or sine waves). We'd really like to begin with an actual dataset of observed
;; $(x, y)$ points, and infer the corresponding slope and intercept (or phase,
;; period, and amplitude). This task is called _posterior inference_.

;; We now will provide a data set of y-coordinates and try to draw inferences
;; about the process that generated the data. We begin with the following data
;; set:

(def ys [6.75003, 6.1568, 4.26414, 1.84894, 3.09686, 1.94026, 1.36411, -0.83959, -0.976, -1.93363, -2.91303])

{::clerk/visibility {:result :hide}}

(defn scatter-spec
  [xs ys & {:keys [color fill-opacity stroke-opacity title y-domain]}]
  (cond-> {:schema "https://vega.github.io/schema/vega-lite/v5.json"
           :embed/opts {:actions false}
           :data {:values (map (fn [x y]
                                 {:x x :y y})
                               xs
                               ys)}
           :mark {:type "circle"}
           :encoding {:x {:field :x
                          :type "quantitative"
                          :title "X"
                          :scale {:zero false}}
                      :y {:field :y
                          :type "quantitative"
                          :title "Y"
                          :axis {:titleAngle 0}
                          :scale {:zero false}}}}
    title (assoc :title title)
    color (assoc-in [:encoding :color] {:value color})
    fill-opacity (assoc-in [:mark :fillOpacity] fill-opacity)
    stroke-opacity (assoc-in [:mark :strokeOpacity] stroke-opacity)
    y-domain (assoc-in [:encoding :y :scale :domain] y-domain)))

(def scatter (comp clerk/vl scatter-spec))

{::clerk/visibility {:result :show}}

(scatter xs ys :title "observed data (linear)")


;; We will assume that the line model was responsible for generating the data,
;; and infer values of the slope and intercept that explain the data.

;; To do this, we write a simple *inference program* that takes the model we are
;; assuming generated our data, the data set, and the amount of computation to
;; perform, and returns a trace of the function that is approximately sampled
;; from the _posterior distribution_ on traces of the function, given the
;; observed data. That is, the inference program will try to find a trace that
;; well explains the dataset we created above. We can inspect that trace to find
;; estimates of the slope and intercept of a line that fits the data.

;; Functions like `gen.inference.importance/importance-resampling` expect us to
;; provide a _model_ and also an _choice map_ representing our data set and
;; relating it to the model.  A choice map maps random choice addresses from the
;; model to values from our data set. Here, we want to tie model addresses like
;; `[:y, 4]` to data set values like `(nth ys 4)`:
;;
;; ```clojure
;; (require '[gen.inference.importance :as importance])
;; ```

(defn do-inference
  [model xs ys amount-of-computation]
  ;; Create a choice map that maps model addresses `[:y, i]` to observed values
  ;; `(nth ys i)`. We leave :slope and :intercept unconstrained, because we want
  ;; them to be inferred.
  (let [observations (reduce (fn [observations [i y]]
                               (assoc observations [:y i] y))
                             (choicemap {})
                             (map-indexed vector ys))]
    (:trace (importance/resampling model [xs] observations amount-of-computation))))

;; We can run the inference program to obtain a trace, and then visualize the result:

(def inference-trace (do-inference line-model xs ys 100))

{::clerk/visibility {:result :show}}

(render-trace inference-trace)

;; We see that `importance_resampling` found a reasonable slope and intercept to
;; explain the data. We can also visualize many samples in a grid:

(grid render-trace (repeatedly 12 #(do-inference line-model xs ys 100)))

;; We can see here that there is some uncertainty: with our limited data, we
;; can't be 100% sure exactly where the line is. We can get a better sense for
;; the variability in the posterior distribution by visualizing all the traces
;; in one plot, rather than in a grid. Each trace is going to have the same
;; observed data points, so we only plot those once, based on the values in the
;; first trace:

^{::clerk/visibility {:result :hide}}
(defn overlay
  [traces]
  (clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
             :embed/opts {:actions false}
             :layer (mapv render-trace-spec traces)}))

(overlay (repeatedly 10 #(do-inference line-model xs ys 100)))

;; ### 3.1 Exercise

;; The results above were obtained with `amount-of-computation` set to `100`.
;; Run the algorithm with this value set to `1`, `10`, and `1000`, etc.  Which
;; value seems like a good tradeoff between accuracy and running time? Discuss.

;; ### 3.2 Exercise

;; Consider the following data set.

(def ys-sine [2.89 2.22 -0.612 -0.522 -2.65 -0.133 2.70 2.77 0.425 -2.11 -2.76])

(scatter xs ys-sine)

;; Write an inference program that generates traces of `sine-model` that explain
;; this data set. Visualize the resulting distribution of traces. Temporarily
;; change the prior distribution on the period to be `(gamma 1 1)`  (by changing
;; and re-evaluating the definition of `sine-model` from a previous exercise).
;; Can you explain the difference in inference results when using `(gamma 1  1)`
;; vs `(gamma 5 1)` prior on the period? How much computation did you need to
;; get good results?

;; ## 4. Predicting new data

;; What if we'd want to predict `ys` given `xs`?

;; Using the API method `gen.generative-function/generate`, we can generate a
;; trace of a generative function in which the values of certain random choices
;; are constrained to given values. The constraints are a choice map that maps
;; the addresses of the constrained random choices to their desired values.

;; For example:

(def predicting-constraints (choicemap {:slope 0 :intercept 0}))
(def predicting-trace (:trace (gf/generate line-model [xs] predicting-constraints)))

(def predict-opts
  {:x-domain [-5 5]
   :y-domain [-5 5]
   :clip true})

(render-trace predicting-trace predict-opts)

;; Note that the random choices corresponding to the y-coordinates are still
;; made randomly. We can generate more traces to verify this.

(->> (repeatedly 4 #(:trace (gf/generate line-model [xs] predicting-constraints)))
     (grid #(render-trace % predict-opts)))

;; We will use the ability to run constrained executions of a generative
;; function to predict the value of the y-coordinates at new x-coordinates by
;; running new executions of the model generative function in which the random
;; choices corresponding to the parameters have been constrained to their
;; inferred values.  We have provided a function below (`predict-new-data`) that
;; takes a trace, and a vector of new x-coordinates, and returns a vector of
;; predicted y-coordinates corresponding to the x-coordinates in `new-xs`. We
;; have designed this function to work with multiple models, so the set of
;; parameter addresses is an argument (`param-addrs`):

(defn predict-new-data
  [model trace new-xs param-addrs]
  ;; Copy parameter values from the inferred trace (`trace`) into a fresh set of
  ;; constraints.
  (let [constraints (reduce (fn [cm param-addr]
                              (assoc cm param-addr (get trace param-addr)))
                            (choicemap {})
                            param-addrs)

        ;; Run the model with new x coordinates, and with parameters
        ;; fixed to be the inferred values.
        {new-trace :trace} (gf/generate model [new-xs] constraints)]

    ;; Pull out the y-values and return them.
    (mapv #(get new-trace [:y %])
          (range (count new-xs)))))

;; To illustrate, we call the function above given the previous trace (which
;; constrained slope and intercept to be zero).

(predict-new-data line-model predicting-trace [1.0 2.0 3.0] [:slope :intercept])

;; The cell below defines a function that first performs inference on an
;; observed data set (`xs`, `ys`), and then runs `predict-new-data` to generate
;; predicted y-coordinates. It repeats this process `num-traces` times, and
;; returns a vector of the resulting y-coordinate vectors.

(defn infer-and-predict
  [model xs ys new-xs param-addrs num-traces amount-of-computation]
  (repeatedly num-traces
              #(let [trace (do-inference model xs ys amount-of-computation)]
                 (predict-new-data model trace new-xs param-addrs))))

;; To illustrate, we generate predictions at `[1.0 2.0 3.0]` given one
;; (approximate) posterior trace.

(infer-and-predict line-model xs ys [1.0 2.0 3.0] [:slope :intercept] 1 1000)

;; Finally, we define a cell that plots the observed data set (`xs`, `ys`) as
;; red dots, and the predicted data as small black dots.

^{::clerk/visibility {:result :hide}}
(defn plot-predictions
  [xs ys new-xs pred-ys & {:keys [title]}]
  (let [points (fn [xs ys type order opacity]
                 (mapv (fn [x y]
                         {:x x :y y :type type :order order :opacity opacity})
                       xs
                       ys))]
    (clerk/vl
     (cond-> {:schema "https://vega.github.io/schema/vega-lite/v5.json"
              :embed/opts {:actions false}
              :data {:values (into (points xs ys :observed 1 1.0)
                                   (mapcat (fn [ys]
                                             (points new-xs ys :predicted 0 0.1))
                                           pred-ys))}
              :mark {:type "circle"}
              :encoding {:x {:field :x :type "quantitative" :title "X" :scale {:zero false}}
                         :y {:field :y :type "quantitative" :title "Y" :scale {:zero false}}
                         :color {:field :type :type "nominal" :legend {:title false}}
                         :order {:field :order}
                         :opacity {:field :opacity :type "quantitative" :legend false}}}
       title (assoc :title title)))))

;; Recall the original dataset for the line model. The x-coordinates span the
;; interval `-5` to `5.`

(scatter xs ys :title "observed data")

;; We will use the inferred values of the parameters to predict y-coordinates
;; for x-coordinates in the interval `5` to `10` from which data was not
;; observed.  We will also predict new data within the interval `-5` to `5`, and
;; we will compare this data to the original observed data. Predicting new data
;; from inferred parameters, and comparing this new data to the observed data is
;; the core idea behind *posterior predictive checking*. This tutorial does not
;; intend to give a rigorous overview behind techniques for checking the quality
;; of a model, but intends to give high-level intuition.

(def new-xs (vec (range -5 10 (/ 15.0 100))))

;; We generate and plot the predicted data:

(def pred-ys (infer-and-predict line-model xs ys new-xs [:slope :intercept] 20 1000))
(plot-predictions xs ys new-xs pred-ys)

;; The results look reasonable, both within the interval of observed data and in
;; the extrapolated predictions on the right.

;; Now consider the same experiment run with the following data set, which has
;; significantly more noise.

(def ys-noisy [5.092 4.781 2.46815 1.23047 0.903318 1.11819 2.10808 1.09198 0.0203789 -2.05068 2.66031])

(let [pred-ys (infer-and-predict line-model xs ys-noisy new-xs [:slope :intercept] 20 1000)]
  (plot-predictions xs ys-noisy new-xs pred-ys))

;; It looks like the generated data is less noisy than the observed data in the
;; regime where data was observed, and it looks like the forecasted data is too
;; overconfident. This is a sign that our model is mis-specified. In our case,
;; this is because we have assumed that the noise has value `0.1`. However, the
;; actual noise in the data appears to be much larger. We can correct this by
;; making the noise a random choice as well and inferring its value along with
;; the other parameters.

;; We first write a new version of the line model that samples a random choice
;; for the noise from a `(dist/gamma 1 1)` prior distribution.

^{::clerk/visibility {:result :hide}}
(def line-model-fancy
  (gen [xs]
    (let [slope (dynamic/trace! :slope dist/normal 0 1)
          intercept (dynamic/trace! :intercept dist/normal 0 2)
          y (fn [x]
              (+ (* slope x)
                 intercept))
          noise (dynamic/trace! :noise dist/gamma 1 1)]
      (doseq [[i x] (map-indexed vector xs)]
        (dynamic/trace! [:y i] dist/normal (y x) noise))
      y)))

;; Then, we compare the predictions using inference of the unmodified and
;; modified models on the `ys` data set:

(clerk/row
 (let [pred-ys (infer-and-predict line-model xs ys new-xs [:slope :intercept] 20 1000)]
   (plot-predictions xs ys new-xs pred-ys :title "fixed noise"))
 (let [pred-ys (infer-and-predict line-model-fancy xs ys new-xs [:slope :intercept] 20 1000)]
   (plot-predictions xs ys new-xs pred-ys :title "inferred noise")))

;; Notice that there is more uncertainty in the predictions made using the
;; modified model.

;; We also compare the predictions using inference of the unmodified and
;; modified models on the `ys-noisy` data set:

(clerk/row
 (let [pred-ys (infer-and-predict line-model xs ys-noisy new-xs [:slope :intercept] 20 1000)]
   (plot-predictions xs ys-noisy new-xs pred-ys :title "fixed noise"))
 (let [pred-ys (infer-and-predict line-model-fancy xs ys-noisy new-xs [:slope :intercept] 20 1000)]
   (plot-predictions xs ys-noisy new-xs pred-ys :title "inferred noise")))

;; Notice that while the unmodified model was very overconfident, the modified
;; model has an appropriate level of uncertainty, while still capturing the
;; general negative trend.

;; ### 4.1 Exercise

^{::clerk/visibility {:code :hide}}
(callout/warning
 "This exercise requires that you have implemented `sine-model` in exercise 2.1.
 Be sure to complete that exercise before attempting this one.")

;; Write a modified version of the sine model that makes noise into a random
;; choice. Compare the predicted data with the observed data using
;; `infer-and-predict` and `plot-predictions` for the unmodified and modified
;; models, and for the `ys-sine` and `ys-noisy` data sets. Discuss the results.
;; Experiment with the amount of inference computation used. The amount of
;; inference computation will need to be higher for the model with the noise as
;; a random choice.

;; We have provided you with starter code:

^{::clerk/visibility {:result :hide}}
(def sine-model-fancy
  (gen [xs]
    ;; < your code here >
    (let [y (fn [x]
              ;; < your code here >
              x)]
      (doseq [[i x] (map-indexed vector xs)]
        (dynamic/trace! [:y i] dist/normal (y x) 0.1))
      y)))

;; Experiment with the vaue of `ex-4-1-computation` below.

(def ex-4-1-computation 2)

(let [pred-ys (infer-and-predict sine-model xs ys-sine new-xs [] 20 ex-4-1-computation)
      pred-ys-fancy (infer-and-predict sine-model-fancy xs ys-sine new-xs [] 20 ex-4-1-computation)]
  (clerk/row
   (plot-predictions xs ys-sine new-xs pred-ys :title ["ys-sine" "fixed noise level"])
   (plot-predictions xs ys-sine new-xs pred-ys-fancy :title ["ys-sine" "inferred noise level"])))

(let [pred-ys (infer-and-predict sine-model xs ys-noisy new-xs [] 20 ex-4-1-computation)
      pred-ys-fancy (infer-and-predict sine-model-fancy xs ys-noisy new-xs [] 20 ex-4-1-computation)]
  (clerk/row
   (plot-predictions xs ys-noisy new-xs pred-ys :title ["ys-noisy" "fixed noise level"])
   (plot-predictions xs ys-noisy new-xs pred-ys-fancy :title ["ys-noisy" "inferred noise level"])))

;; **Solution**

^{::clerk/visibility {:code :fold :result :hide}}
(comment
  (def sine-model-fancy
    (gen [xs]
      (let [period (dynamic/trace! :period dist/gamma 5 1)
            amplitude (dynamic/trace! :amplitude dist/gamma 1 1)
            phase (dynamic/trace! :phase dist/uniform 0 (* 2 math/PI))
            noise (dynamic/trace! :noise dist/gamma 1 1)
            y (fn [x]
                (* amplitude
                   (math/sin (+ (* x
                                   (/ (* 2 math/PI)
                                      period))
                                phase))))]
        (doseq [[i x] (map-indexed vector xs)]
          (dynamic/trace! [:y i] dist/normal (y x) noise)))
      y)))

;; ## 5. Calling other generative functions

;; In addition to making random choices, generative functions can invoke other
;; generative functions. To illustrate this, we will write a probabilistic model
;; that combines the line model and the sine model. This model is able to
;; explain data using either model, and which model is chosen will depend on the
;; data. This is called *model selection*.

;; A generative function can invoke another generative function in ~~three~~ two
;; ways:

;; 1. **(NOT RECOMMENDED)** using regular Clojure function call syntax: `(f x)`
;; 2. using `dynamic/trace!` with an address for the call: `(dynamic/trace! :addr f x)`
;; 3. using `dynamic/splice!`, which does not require an address: `(dynamic/splice! f x)`

;; When invoking using regular function call syntax, the random choices made by
;; the callee function are not traced at all, and Gen cannot reason about them
;; during inference. When invoking using `dynamic/splice!` the random choices of
;; the callee function are imported directly into the caller's trace. So, for
;; example, if `f` makes a choice called `:f-choice`, then the caller's trace
;; will have a choice called `:f-choice` too. Note that a downside of this is
;; that if `f` is called _twice_ by the same caller, then the two choices called
;; `:f-choice` will clash, leading to an error. In this case, it is best to
;; provide an address (`(dynamic/trace! addr f)`): `f`'s random choices will
;; be placed under the _key_ `addr`.

(def foo
  (gen []
    (dynamic/trace! :y dist/normal 0 1)))

(def bar
  (gen []
    (dynamic/trace! :x dist/bernoulli 0.5)
    ;; Call `foo` with `dynamic/splice!`. Its choices (`:y`) will appear directly
    ;; within the trace of `bar`.
    (dynamic/splice! foo)))

(def bar-with-key
  (gen []
    (dynamic/trace! :x dist/bernoulli 0.5)
    ;; Call `foo` with the address `:z`.  The internal choice `:y` of `foo` will
    ;; appear in our trace at the hierarchical address `[:z :y]`.
    (dynamic/trace! :z foo)))

;; We first show the addresses sampled by `bar`:

(trace/get-choices (gf/simulate bar []))

;; And the addresses sampled by `bar-with-key`:

(def bar-with-key-trace (gf/simulate bar-with-key []))
(trace/get-choices bar-with-key-trace)

;; Using `dynamic/trace!` instead of `dynamic/splice!` can help avoid address
;; collisions for complex models.

;; Hierarchical traces are represented using nested choice maps
;; (`gen.dynamic.choicemap/ChoiceMap`). Hierarchical addresses can be accessed
;; using `clojure.core` functions like `clojure.core/get-in`.

(get-in bar-with-key-trace [:z :y])

;; Now, we write a generative function that combines the line and sine models.
;; It makes a Bernoulli random choice (e.g. a coin flip that returns true or
;; false) that determines which of the two models will generate the data.

(def combined-model
  (gen [xs]
    (if (dynamic/trace! :is-line dist/bernoulli 0.5)
      ;; Call `line-model-fancy` on xs, and import its random choices directly
      ;; into our trace.
      (dynamic/splice! line-model-fancy xs)
      ;; Call `sine-model-fancy` on xs, and import its random choices directly
      ;; into our trace.
      (dynamic/splice! sine-model-fancy xs))))

;; We visualize some traces, and see that sometimes it samples linear data and
;; other times sinusoidal data.

(let [traces (repeatedly 12 #(gf/simulate combined-model [xs]))]
  (grid render-trace traces))

;; We run inference using this combined model on the `ys` data set and the
;; `ys-sine` data set.

#_
(let [amount-of-computation 10000
      ys-traces (repeatedly 10 #(do-inference combined-model xs ys amount-of-computation))
      ys-sine-traces (repeatedly 10 #(do-inference combined-model xs ys-sine amount-of-computation))]
  (clerk/row
   (overlay ys-traces)
   (overlay ys-sine-traces)))

;; The results should show that the line model was inferred for the `ys` data
;; set, and the sine wave model was inferred for the `ys-sine` data set.

;; ### 5.1 Exercise

;; Construct a data set for which it is ambiguous whether the line or sine wave
;; model is best. Visualize the inferred traces using `render-combined` to
;; illustrate the ambiguity. Write a program that takes the data set and returns
;; an estimate of the posterior probability that the data was generated by the
;; sine wave model, and run it on your data set.

^{::clerk/visibility {:code :hide}}
(callout/hint
 "To estimate the posterior probability that the data was generated by the sine
  wave model, run the inference program many times to compute a large number of
  traces, and then compute the fraction of those traces in which `:is-line` is
  false.")

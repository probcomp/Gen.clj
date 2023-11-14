(ns introduction
  {:nextjournal.clerk/toc true}
  (:require [clojure.math :as math]
            [clojure.repl :as repl]
            [gen.distribution.commons-math :as dist]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.generative-function :as gf]
            [gen.trace :as trace]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(set! *print-length* 10)

;; # A bottom-up introduction to Gen.clj

;; This notebook introduces some of the core concepts in Gen from the bottom-up,
;; and uses some mathematical notation.

;; ## 1. A simple probabilistic Clojure program

;; Consider the following Clojure code:

(defn f
  [p]
  (let [n0 (dist/uniform-discrete 1 10)
        n1 (if (dist/bernoulli p)
             (* n0 2)
             n0)]
    (dist/categorical (for [i (range 1 21)]
                        (if (= i n1)
                          0.5
                          (/ 0.5 19))))))

;; The function `f` calls three functions provided by Gen, each of which returns
;; a random value, sampled from a certain probability distribution:

^{::clerk/visibility {:code :hide :result :hide}}
(defmacro doc
  [sym]
  `(clerk/html
    {::clerk/width :wide}
    [:pre (with-out-str (repl/doc ~sym))]))

(doc dist/uniform-discrete)
(doc dist/bernoulli)
(doc dist/categorical)

;; These are three of the many probability distributions that are provided by
;; Gen.

;; The function `f` first binds the value of `n0` to a random value drawn from
;; the set of integers `#{1 ... 10}`:

;; ```clojure
;; (dist/uniform-discrete 1 10)
;; ```

;; Then, with probability `p`, it multiplies n by two:
;;
;; ```clojure
;; (if (dist/bernoulli p)
;;   (* n0 2)
;;   n0)
;; ```

;; Then, it samples an integer in the set `#{1 ... 20}`. With probability `0.5`
;; the integer is `n`, and with probability `0.5` it is uniformly chosen from
;; the remaining `19` integers. It returns this sampled integer:

;; ```clojure
;; (dist/categorical (for [i (range 1 21)]
;;                      (if (= i n1)
;;                        0.5
;;                        (/ 0.5 19))))
;; ```

;; If we run this function many times, we can see the probability distribution
;; on its return values. The distribution depends on the argument `p` to the
;; function:

(defn histogram
  [data]
  (clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
             :embed/opts {:actions false}
             :data {:values data}
             :mark "bar"
             :encoding {:x {:bin {:extent [1 20]
                                  :step 1}
                            :field :fp
                            :axis {:title "(f p)"}}
                        :y {:aggregate "count"
                            :axis {:title nil
                                   :labels false
                                   :domain false
                                   :grid false
                                   :ticks false}}
                        :column {:field :p}}}))

(histogram (for [p [0.1 0.5 0.9]
                 _ (range 1000)]
             {:p p
              :fp (f p)}))

;; Suppose we wanted to see what the distribution on return values would be if
;; value of `n0` were `2.` Because we don't know what random values were sampled
;; during a given execution, we can't use simulations of `f` to answer this
;; question. We would have to modify `f` first, to return `n0`:

(defn f-with-n0
  [p]
  (let [n0 (dist/uniform-discrete 1 10)
        n1 (if (dist/bernoulli p)
             (* n0 2)
             n0)
        fp (dist/categorical (for [i (range 1 21)]
                               (if (= i n1)
                                 0.5
                                 (/ 0.5 19))))]
    {:n0 n0
     :fp fp}))

;; Then, we could only include executions in which our desired events did
;; happen, when making our histogram:

(histogram (apply concat
                  (for [p [0.1 0.5 0.9]]
                    (->> (repeatedly #(f-with-n0 p))
                         (filter #(= 2 (:n0 %)))
                         (take 1000)
                         (map :fp)
                         (map (fn [fp]
                                {:p p
                                 :fp fp}))))))

;; Suppose we wanted to ask more questions. We might need to modify each time we
;; have a new question, to make sure that the function returns the particular
;; pieces of information about the execution that the question requires.

;; Note that if the function always returned the value of *every random choice*,
;; then these values are sufficient to answer any question using executions of
;; the function, because all states in the execution of the function are
;; deterministic given the random choices. We will call the record of all the
;; random choies a **trace**. In order to store all the random choices in the
;; trace, we need to come up with a unique name or **address** for each random
;; choice.

;; Below, we implement the trace as a dictionary that maps addresses of random
;; choices to their values. We use a unique Julia Symbol for each address:

(defn f-with-trace
  [p]
  (let [trace (atom {})
        n0 (dist/uniform-discrete 1 10)
        _ (swap! trace assoc :n0 n0)
        if-test (dist/bernoulli p)
        _ (swap! trace assoc :if-test if-test)
        n1 (if if-test
             (* n0 2)
             n0)
        fp (dist/categorical (for [i (range 1 21)]
                               (if (= i n1)
                                 0.5
                                 (/ 0.5 19))))
        _ (swap! trace assoc :fp fp)]
    {:trace @trace
     :fp fp}))

;; We run the function, and get the return value and the trace:

(f-with-trace 0.3)

;; However, this program looks more complicated than the original program. We
;; could make the syntax for tracing more concise:

(defn trace-assoc!
  [trace address value]
  (swap! trace assoc address value)
  value)

(defn f-with-trace-improved
  [p]
  (let [trace (atom {})
        n0 (trace-assoc! trace :n0 (dist/uniform-discrete 1 10))
        n1 (if (trace-assoc! trace :if-test (dist/bernoulli p))
             (* n0 2)
             n0)
        fp (trace-assoc! trace
                         :fp
                         (dist/categorical (for [i (range 1 21)]
                                             (if (= i n1)
                                               0.5
                                               (/ 0.5 19)))))
        _ (swap! trace assoc :fp fp)]
    {:trace @trace
     :fp fp}))

;; We run the function, and get the return value and the trace:

(f-with-trace-improved 0.3)

;; Now that we have instrumented the function, we can answer the following
;; different question without needing to modify the function:

;; > What is the probability that the branch was taken, given that the result
;; > took the value `4`?

(defn query
  [ps observed-fp]
  (let [data (apply concat
                    (for [p ps]
                      (->> (repeatedly #(f-with-trace-improved p))
                           (filter (fn [{:keys [trace]}]
                                     (= observed-fp (get trace :fp))))
                           (take 1000)
                           (mapv (fn [{:keys [trace]}]
                                   {:p p
                                    :if-test (get trace :if-test)})))))]
    (clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
               :embed/opts {:actions false}
               :data {:values data}
               :mark "bar"
               :encoding {:x {:field :if-test
                              :axis {:title ":if-test"}}
                          :y {:aggregate "count"
                              :axis {:title nil
                                     :labels false
                                     :domain false
                                     :grid false
                                     :ticks false}}
                          :column {:field :p}}})))

(query [0.1 0.5 0.9] 4)

;; > What about a result value that is greater than `10`?

(query [0.1 0.5 0.9] 14)

;; ## 2. Tracing the values of random choices in generative functions

;; The ability to trace the values of random choices in a probabilistic program
;; (i.e. record the value of each choice in a trace data structure) is one of
;; the basic features of Gen's built-in modeling language. To write a function
;; in this language we use the `gen.dynamic/gen` macro provided by Gen.
;;
;; ```clojure
;; (require '[gen.dynamic :as dynamic :refer [gen]])
;; ```

(def gen-f
  (gen [p]
    (let [n0 (dynamic/trace! :n0 dist/uniform-discrete 1 10)
          n1 (if (dynamic/trace! :if-test dist/bernoulli p)
               (* n0 2)
               n0)]
      (dynamic/trace! :fp dist/categorical (for [i (range 1 21)]
                                             (if (= i n1)
                                               0.5
                                               (/ 0.5 19)))))))

;; The `(dynamic/trace! address distribution args...)` expression records the
;; value of the given random choice at the given address into an implicit trace
;; data structure. The trace data structure itself is not a variable in the
;; function, and that code in the body of the function cannot read from the
;; trace. It is an error to use this syntax with the same address twice.
;; Addresses can be arbitrary Clojure values. In this notebook, all the
;; addresses will be Clojure keywords.

;; Also note that the trace is not part of the return value:

(gen-f 0.3)

;; To run a `gen.dynamic/gen` function and get a trace of the execution, we use
;; the simulate function:

(def genf-trace (gf/simulate gen-f [0.3]))

;; We can access the values of random choices by indexing into the trace:

(get genf-trace :n0)

;; We can also print the values of all choices made:

(trace/get-choices genf-trace)

;; Gen also stores the arguments on which the function was called:

(trace/get-args genf-trace)

;; And the return value:

(trace/get-retval genf-trace)

;; Now, we will answer the same question as above, but this time using our
;; `gen.dynamic/gen` function:

(defn gen-query
  [ps observed-fp]
  (let [data (apply concat
                    (for [p ps]
                      (->> (repeatedly #(trace/get-choices (gf/simulate gen-f [p])))
                           (filter (fn [trace]
                                     (= observed-fp (get trace :fp))))
                           (take 1000)
                           (mapv (fn [trace]
                                   {:p p
                                    :if-test (get trace :if-test)})))))]
    (clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
               :embed/opts {:actions false}
               :data {:values data}
               :mark "bar"
               :encoding {:x {:field :if-test
                              :axis {:title ":if-test"}}
                          :y {:aggregate "count"
                              :axis {:title nil
                                     :labels false
                                     :domain false
                                     :grid false
                                     :ticks false}}
                          :column {:field :p}}})))

(gen-query [0.1 0.5 0.9] 14)

;; ## 3. The probability distribution represented by a generative function

;; A generative function that terminates with probability one has a probability
;; distribution on its executions. We represent an execution of the function as
;; the map from addresses of random choices to their values. We call this map a
;; choice map (denoted mathematically by $t$). Then, for given arguments to the
;; function (denoted mathematically by $x$), we can list the possible choice
;; maps, and we can compute the probability of each choice map (denoted
;; $p(t;x)$) by taking the product of the probability of each random choice in
;; the map. We can also compute the return value of the function from the
;; arguments and the choice map (the function that computes the return value is
;; denoted mathematically by $f(x;t)$). Let's do this for a simple function
;; `foo`:

(def foo
  (gen [prob-a]
    (let [a-b (or (not (dynamic/trace! :a dist/bernoulli prob-a))
                  (dynamic/trace! :b dist/bernoulli 0.6))
          prob-c (if a-b 0.9 0.2)]
      (and (dynamic/trace! :c dist/bernoulli prob-c)
           a-b))))

(trace/get-choices (gf/simulate foo [0.3]))

^{::clerk/visibility {:code :hide}}
(clerk/html
 [:table
  [:thead
   [:tr
    [:th (clerk/md "Random choice map $t$")]
    [:th (clerk/md "Probability $p(t;x)$")]
    [:th (clerk/md "Return value $f(x,t)$")]]]
  [:tbody
   [:tr
    [:td (clerk/code {:a true :b true :c true})]
    [:td (clerk/code '(* prob-a 0.6 0.9))]
    [:td (clerk/code true)]]
   [:tr
    [:td (clerk/code {:a true :b true :c false})]
    [:td (clerk/code '(* prob-a 0.6 0.1))]
    [:td (clerk/code false)]]
   [:tr
    [:td (clerk/code {:a true :b false :c true})]
    [:td (clerk/code '(* prob-a 0.4 0.2))]
    [:td (clerk/code false)]]
   [:tr
    [:td (clerk/code {:a true :b false :c false})]
    [:td (clerk/code '(* prob-a 0.4 0.8))]
    [:td (clerk/code false)]]
   [:tr
    [:td (clerk/code {:a false :c true})]
    [:td (clerk/code '(* (- 1 prob-a) 0.9))]
    [:td (clerk/code true)]]
   [:tr
    [:td (clerk/code {:a false :c false})]
    [:td (clerk/code '(* (- 1 prob-a) 0.1))]
    [:td (clerk/code false)]]]])

;; Check your understanding by deriving by hand the probability and return value
;; for a few rows of this table.

;; Based on our table, the probability that `foo` returns `true` is:

(defn prob-true
  [prob-a]
  (+ (* prob-a
        0.6
        0.9)
     (* (- 1 prob-a)
        0.9)))

;; Let's check that using some simulations:

(clerk/table
 #::clerk{:width :prose}
 (for [prob-a [0.1 0.5 0.9]]
   (let [total 10000
         expected (prob-true prob-a)
         actual (/ (-> (repeatedly total #(foo prob-a))
                       (frequencies)
                       (get true))
                   total)]
     {:expected expected
      :actual actual})))

;; We can also get the log probability that an individual trace would be
;; generated by the function $logp(t;x)$, using the `trace/get-score` function.

;; Let's generate a trace below, get its log probability with `trace/get-score`:

(def trace2 (gf/simulate foo [0.3]))

(trace/get-choices trace2)

(trace/get-score trace2)

(math/exp (trace/get-score trace2))

;; Check this value against the hand-computed value in our table above.

;; ## 4. Generating a trace that satisfies certain constraints

;; So far, we have run generative functions in two ways:

;; (1) Using the usual Clojure call syntax:

(gen-f 0.3)

;; (2) Using the `gen.generative-function/simulate` function:

(gf/simulate gen-f [0.3])

;; We can also generate a trace that satisfies a set of constraints on the valus
;; of random choices using the generate function. Suppose that we want a trace
;; where `:a` is always true and `:c` is always false. We first construct a
;; choice map containing these constraints:

(require '[gen.dynamic.choice-map :as dynamic.choice-map]
         '[gen.choice-map :as choice-map])

(def constraints
  (dynamic.choice-map/choice-map
   :a true
   :c false))

#_
(choice-map/submaps
 (dynamic.choice-map/choice-map
  :a true
  :c false))

;; The `gen.dynamic.choice-map/choice-map` constructor above took two elements
;; of the form (address, value). This is equivalent to constructing an empty
;; choice map and then populating it:

(def choices
  (assoc (dynamic.choice-map/choice-map)
         :a true
         :c false))

(choice-map/submaps choices)

;; Then, we pass the constraints as the third argument to
;; `gen.generative-function/generate`, after the function itself and the
;; arguments:

(def result (gf/generate foo [0.3] constraints))
(def trace (:trace result))
(def weight (:weight result))

;; Note that we also get a weight in addition to the trace. We will discuss the
;; weight shortly.

;; Let’s check that the trace actually agrees with our constraints:

(trace/get-choices trace)

;; We can also check the return value:

(trace/get-retval trace)

;; When we invoke `gen.generative-function/generate`, the choice map is clearly
;; not being sampled from $p(t; x)$, because $p(t; x)$ can generate 6 possible
;; choice maps, whereas our call to generate can only generate 2 possible choice
;; maps. Instead, the generative function employs an **internal proposal
;; distribution** on choice maps $t$, denoted mathematically by $q(t; x, u)$,
;; where $u$ is the choice map for the constraints.

;; The internal proposal distribution cannot generate a choice map that
;; disagrees with the constraints, and it can only generate choice maps that
;; could possibly be sampled from $p(t; x)$. Furthermore, we also require the
;; internal proposal distribution to have some probability of sampling any
;; choice map that agrees with the constraints, and that could be possibly be
;; sampled from $p(t; x)$. These requirements can be summarized by the
;; following:

;; $q(t; x, u) > 0 \Longleftrightarrow p(t; x) > 0 \land u(a) = t(a) \;\; \forall a \in dom(u) \cap dom(t)$

;; where $\text{dom}$ stands for 'domain', and gives the set of addresses in a choice map.

;; The specific internal proposal distribution used by `gen.dynamic/gen`
;; functions is based on **ancestral sampling**, which operates as follows: We
;; run the function. To evaluate a `dynamic/trace!` expression, we look up the
;; address in the constraints choice map. If the address is present in the
;; constraints choice map, we deterministically return the value stored in the
;; constraints for that address. If the address is not present in the
;; constraints, we sample the value from the distribution in the
;; `dynamic/trace!` expression. For the function `foo`, with constraints
;; $u = \{a \mapsto \text{true}, c \mapsto \text{false}\}$,
;; the internal proposal distribution is:

^{::clerk/visibility {:code :hide}}
#_
(clerk/tex "
\\begin{array}{l|l}
\\text{Random choice map } t & q(t; x, u)\\\\
\\hline
\\{a \\mapsto \\text{true}, b \\mapsto \\text{true}, c \\mapsto \\text{false}\\} & 0.6\\\\
\\{a \\mapsto \\text{true}, b \\mapsto \\text{false}, c \\mapsto \\text{false}\\} & 0.4
\\end{array}
")

;; Check your understanding by deriving this distribution by hand.

;; The weight returned by `generate` is:

;; $$\log \frac{p(t; x)}{q(t; x, u)}$$

;; Let's confirm this using our trace and weight sampled above. Note that we ran
;; `generate` with `{:prob-a 0.3}`:

(let [expected (if (get trace :b)
                 ;; choice map is {:a true :b true :c false}
                 (math/log (/ (* 0.3 0.6 0.1)
                              0.6))
                 ;; choice map is {:a true :b true :c false}
                 (math/log (/ (* 0.3 0.4 0.8)
                              0.4)))]
  (clerk/table (clerk/use-headers [["expected" "actual"]
                                   [expected weight]])))

;; The ability to generate a trace that satisfies constraints, along with the
;; weight, is a useful primitive operation for implementing a class of
;; approximate inference algorithms called **importance resampling**. We can
;; implement importance resampling by (i) generating a collection of traces
;; satisfying the constraints, and associated weights, and (ii) returning one of
;; these traces with probability in proportion to its weight:

(defn my-importance-sampler
  [gf args constraints num-particles]
  (let [results (repeatedly num-particles #(gf/generate gf args constraints))
        traces (map :trace results)
        ;; note: the weights are in log-space, so we exponentiate
        weights (map (comp math/exp :weight) results)
        sum (reduce + weights)
        normalized-weights (map #(/ % sum) weights)
        i (dist/categorical normalized-weights)]
    (nth traces i)))

;; A more efficient and numerically robust implementation of importance
;; resampling is provided in Gen's inference library.

;; Suppose our goal is to sample `:a` and `:b` from the conditional distribution
;; given that we have observed `:c` is `false`. That is, we want to sample
;; choice map $t$ with probability $0$ if $t(c) = \text{false}$ and otherwise
;; probability:

;; $$\frac{p(t; x)}{\displaystyle \sum_{t' : t'(c) = \text{true}} p(t'; x)}$$

;; In this simple case, we can compute the probability by hand (assuming `(=
;; prob-a 0.3)`). There are three choice maps with nonzero probability:

(def p1 (* 0.3 0.6 0.1))
(def p2 (* 0.3 0.4 0.8))
(def p3 (* 0.7 0.1))

[(/ p1 (+ p1 p2 p3))
 (/ p2 (+ p1 p2 p3))
 (/ p3 (+ p1 p2 p3))]

^{::clerk/visibility {:code :hide}}
(clerk/tex "
\\begin{array}{l|l}
\\text{Random choice map } t & \\text{Conditional probability }\\\\
\\hline
\\{a \\mapsto \\text{true}, b \\mapsto \\text{true}, c \\mapsto \\text{true}\\} & 0\\\\
\\{a \\mapsto \\text{true}, b \\mapsto \\text{true}, c \\mapsto \\text{false}\\} & 0.0978\\\\
\\{a \\mapsto \\text{true}, b \\mapsto \\text{false}, c \\mapsto \\text{true}\\} & 0\\\\
\\{a \\mapsto \\text{true}, b \\mapsto \\text{false}, c \\mapsto \\text{false}\\} & 0.5217\\\\
\\{a \\mapsto \\text{false}, c \\mapsto \\text{true}\\} &  0\\\\
\\{a \\mapsto \\text{false}, c \\mapsto \\text{false}\\} & 0.3804
\\end{array}
")

;; In particular, the probability that `:a` is `true` given our condition, is:

(def prob-a-true
  (/ (+ p1 p2)
     (+ p1 p2 p3)))

;; We can sample approximately from this disribution using our importance
;; sampler. As we increase the number of traces, the actual distribution
;; approaches the desired distribution:

(defn draw-samples
  [n-particles constraints]
  (repeatedly 10000 #(-> (my-importance-sampler foo [0.3] constraints n-particles)
                         (trace/get-choices)
                         (get :a))))

(def samples
  (let [n-particles [1 10 100]]
    (zipmap n-particles
            (mapv #(draw-samples % #gen/choice-map {:c false})
                  n-particles))))

(clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
           :embed/opts {:actions false}
           :data {:values (mapcat (fn [[n-particles samples]]
                                    (for [sample samples]
                                      {:n-particles n-particles
                                       :a sample}))
                                  samples)}
           :mark "bar"
           :encoding {:x {:field :a
                          :axis {:labelAngle 0
                                 :labelOverlap false
                                 :labelPadding 6}}
                      :y {:aggregate "count"
                          :axis {:title nil
                                 :labels false
                                 :domain false
                                 :grid false
                                 :ticks false}}
                      :column {:field :n-particles
                               :axis {:title "number of particles"}}}})

;; Indeed, the estimated probability that `a` = `true` is approaching the true
;; probability that we manually computed.

(defn estimate
  [as]
  (let [counts (frequencies as)
        true-count (get counts true 0)
        false-count (get counts false 0)]
    (double (/ true-count (+ true-count false-count)))))

(clerk/vl {:schema "https://vega.github.io/schema/vega-lite/v5.json"
           :embed/opts {:actions false}
           :layer [{:data {:values (for [[n-particles as] samples]
                                     {:n-particles n-particles
                                      :estimate (estimate as)})}
                    :mark "line"
                    :encoding {:x {:field :n-particles
                                   :type "quantitative"
                                   :scale {:type "log"}
                                   :axis {:title "number of particles"}}
                               :y {:field :estimate
                                   :type "quantitative"
                                   :scale {:type "linear"
                                           :domain [0.0 1.0]}
                                   :axis {:title ["estimated" "p(a = true)"]
                                          :orient "left"
                                          :titleAngle 0
                                          :titleAlign "right"}}}}
                   {:data {:values (for [[n-particles as] samples]
                                     {:n-particles n-particles
                                      :estimate (estimate as)})}
                    :mark "circle"
                    :encoding {:x {:field :n-particles
                                   :type "quantitative"
                                   :scale {:type "log"}
                                   :axis {:title "number of particles"}}
                               :y {:field :estimate
                                   :type "quantitative"
                                   :scale {:type "linear"
                                           :domain [0.0 1.0]}
                                   :axis {:title ["estimated" "p(a = true)"]
                                          :orient "left"
                                          :titleAngle 0
                                          :titleAlign "right"}}}}
                   {:data {:values [{}]}
                    :mark {:type "rule" :color "red"}
                    :encoding {:y {:datum prob-a-true
                                   :axis {:orient "right"
                                          :format ".16"
                                          :values [0.0 prob-a-true 1.0]}}}}]})

;; ## 5. Updating a trace

;; Gen also provides a primitive for updating a trace to conform with new
;; constraints on its choice map. We can use this to implement iterative
;; inference and local optimization algorithms.

;; Consider the function `foo` from above. Let's obtain an initial trace:

(def update-trace (:trace (gf/generate foo [0.3] #gen/choice-map {:a true :b true :c true})))
(trace/get-choices update-trace)

;; Now, we use the `update` function, to change the value of `:c` from `true` to
;; `false`:

(def updated (trace/update update-trace #gen/choice-map {:c false}))
(trace/get-choices (:trace updated))

;; The `update` function returns the new trace, as well as a weight, which the
;; log ratio of probabilities of the new choice map ($t'$) and the old choice
;; map ($t$):

;; $$\log \frac{p(t'; x')}{p(t; x)}$$

;; The `update` function also allows you to change the arguments to the function
;; (from $x$ to $x'$), but we will not discuss that in this tutorial.

;; Let's confirm that the weight matches a manual calculation:

(def expected-weight
  (- (math/log (* 0.3 0.6 0.1))
     (math/log (* 0.3 0.6 0.9))))

(:weight updated)

;; Doing an update can also cause some addresses to leave the choice map
;; altogether. For example, if we set `:a` to `false`, then choice at address
;; `:b` is no longer include in the choice map.

(def update-a-true (trace/update update-trace #gen/choice-map {:a false}))
(trace/get-choices (:trace update-a-true))

;; The *discard* choice map that is returned by `update` contains the valus for
;; any addresses that were removed from the choice map, as well as any the
;; previous values for any addresses that were constrained:

(def discard (:discard update-a-true))

;; Note that if we now apply the discard as the constraints, we will get back
;; the original choice map:

(def update-with-discard
  (-> update-a-true
      (:trace)
      (trace/update discard)))

(-> update-with-discard
    (:trace)
    (trace/get-choices))

;; The new discard choice map now contains the old constraints:

(:discard update-with-discard)

;; This illustrates a useful notion of **reversibility** of the `update`
;; function, which will be important when using it as a primitive in
;; Metropolis-Hastings algorithms.

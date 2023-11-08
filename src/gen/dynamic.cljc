(ns gen.dynamic
  (:require [clojure.walk :as walk]
            [gen.choice-map :as choice-map]
            [gen.dynamic.trace :as dynamic.trace]
            [gen.generative-function :as gf]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic :refer [untraced]])))

;; TODO move these to `gen`.

(defn trace! [& _]
  {:arglists '([addr f & xs])}
  (throw
   (ex-info "Illegal usage of `trace!` out of `gen`." {})))

(defn splice! [& _]
  {:arglists '([f & xs])}
  (throw
   (ex-info "Illegal usage of `splice!` out of `gen`." {})))

(defmacro untraced
  [& body]
  `(binding [dynamic.trace/*trace* dynamic.trace/no-op]
     ~@body))

(defrecord DynamicDSLFunction [clojure-fn has-argument-grads accepts-output-grad?]
  gf/IGenerativeFunction
  (has-argument-grads [_] has-argument-grads)
  (accepts-output-grad? [_] accepts-output-grad?)
  (get-params [_] ())
  (simulate [gf args]
    (let [!trace (atom (dynamic.trace/trace gf args))]
      (binding [dynamic.trace/*trace*
                (fn
                  ([^DynamicDSLFunction gf args]
                   (apply (.-clojure-fn gf) args))
                  ([k gf args]
                   (dynamic.trace/validate-empty! @!trace k)
                   (let [subtrace (gf/simulate gf args)]
                     (swap! !trace dynamic.trace/add-call k subtrace)
                     (trace/get-retval subtrace))))]
        (let [retval (apply clojure-fn args)
              trace  @!trace]
          (dynamic.trace/with-retval trace retval)))))

  #?@(:clj
      [clojure.lang.IFn
       (invoke [_]
               (untraced (clojure-fn)))
       (invoke [_ x1]
               (untraced (clojure-fn x1)))
       (invoke [_ x1 x2]
               (untraced (clojure-fn x1 x2)))
       (invoke [_ x1 x2 x3]
               (untraced (clojure-fn x1 x2 x3)))
       (invoke [_ x1 x2 x3 x4]
               (untraced (clojure-fn x1 x2 x3 x4)))
       (invoke [_ x1 x2 x3 x4 x5]
               (untraced (clojure-fn x1 x2 x3 x4 x5)))
       (invoke [_ x1 x2 x3 x4 x5 x6]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 s]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 s)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]
               (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)))
       (invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 xs]
               (untraced
                (apply clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 xs)))
       (applyTo [_ xlist]
                (untraced (.applyTo ^clojure.lang.IFn clojure-fn xlist)))]

      :cljs
      [IFn
       (-invoke [_]
                (untraced (clojure-fn)))
       (-invoke [_ x1]
                (untraced (clojure-fn x1)))
       (-invoke [_ x1 x2]
                (untraced (clojure-fn x1 x2)))
       (-invoke [_ x1 x2 x3]
                (untraced (clojure-fn x1 x2 x3)))
       (-invoke [_ x1 x2 x3 x4]
                (untraced (clojure-fn x1 x2 x3 x4)))
       (-invoke [_ x1 x2 x3 x4 x5]
                (untraced (clojure-fn x1 x2 x3 x4 x5)))
       (-invoke [_ x1 x2 x3 x4 x5 x6]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 s]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 s)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20]
                (untraced (clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20)))
       (-invoke [_ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 xs]
                (untraced (apply clojure-fn x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 xs)))]))

;; The following two functions use a brittle form of macro-rewriting; we should
;; really look at the namespace and local macro environments to try and see if a
;; particular symbol is bound to `#'gen.dynamic/{trace!,splice!}`. See
;; https://github.com/InferenceQL/gen.clj/issues/42.

(defn trace-form?
  "Returns true if `form` is a trace form."
  [form]
  (and (seq? form)
       (#{'trace! 'dynamic/trace! 'gen.dynamic/trace!} (first form))))

(defn splice-form?
  "Returns true if `form` is a splice form."
  [form]
  (and (seq? form)
       (#{'splice! 'dynamic/splice! 'gen.dynamic/splice!} (first form))))

(defn ^:no-doc gen-body [& xs]
  (let [name (when (simple-symbol? (first xs))
               (first xs))
        [params & body] (if name (rest xs) xs)
        has-arg-grads (mapv (constantly false) params)
        accepts-output-grad? false]
    `(-> (fn ~@(when name [name])
           ~params
           ~@(walk/postwalk
              (fn [form]
                (cond (trace-form? form)
                      (let [[addr gf & xs] (rest form)]
                        `((dynamic.trace/active-trace) ~addr ~gf [~@xs]))

                      (splice-form? form)
                      (let [[gf & xs] (rest form)]
                        `((dynamic.trace/active-trace) ~gf [~@xs]))

                      :else form))
              body))
         (->DynamicDSLFunction ~has-arg-grads ~accepts-output-grad?))))

(defmacro gen
  "Defines a generative function."
  [& args]
  {:clj-kondo/lint-as 'clojure.core/fn}
  (apply gen-body args))

;; ## Generate impl

(defn ^:no-doc assoc-state
  "combine by adding weights?"
  [state k {:keys [trace weight]}]
  (-> state
      (update :trace dynamic.trace/add-call k trace)
      (update :weight + weight)))

(extend-type DynamicDSLFunction
  gf/IGenerate
  (-generate [gf args constraints]
    (let [trace  (dynamic.trace/trace gf args)
          !state (atom {:trace trace :weight 0.0})]
      (binding [dynamic.trace/*trace*
                (fn
                  ([^DynamicDSLFunction gf args]
                   (apply (.-clojure-fn gf) args))
                  ([k gf args]
                   (dynamic.trace/validate-empty! (:trace @!state) k)
                   (let [{subtrace :trace :as ret}
                         (let [k-constraints (choice-map/get-submap constraints k)]
                           (gf/generate gf args k-constraints))]
                     (swap! !state assoc-state k ret)
                     (trace/get-retval subtrace))))]
        (let [retval (apply (.-clojure-fn gf) args)
              state  @!state]
          (update state :trace dynamic.trace/with-retval retval))))))

;; ## Assess
;;
;; TODO figure out visited / unvisited??

(extend-type DynamicDSLFunction
  gf/IAssess
  (assess [gf args choices]
    (let [!weight (atom 0.0)]
      (binding [dynamic.trace/*trace*
                (fn
                  ([^DynamicDSLFunction gf args]
                   (apply (.-clojure-fn gf) args))
                  ([k gf args]
                   (let [{:keys [weight retval]}
                         (let [k-choices (choice-map/get-submap choices k)]
                           (gf/assess gf args k-choices))]
                     (swap! !weight + weight)
                     retval)))]
        (let [retval (apply (.-clojure-fn gf) args)]
          {:weight @!weight
           :retval retval})))))


;; ## Propose
;;
;; TODO figure out visited / unvisited??

(extend-type DynamicDSLFunction
  gf/IPropose
  (propose [gf args]
    (let [!state (atom {:choices (choice-map/choicemap)
                        :weight  0.0})]
      (binding [dynamic.trace/*trace*
                (fn
                  ([^DynamicDSLFunction gf args]
                   (apply (.-clojure-fn gf) args))
                  ([k gf args]
                   (let [{:keys [submap weight retval]} (gf/propose gf args)]
                     (swap! !state
                            (fn [m]
                              (-> m
                                  (update :choices choice-map/cm:assoc k submap)
                                  (update :weight + weight))))
                     retval)))]
        (let [retval (apply (.-clojure-fn gf) args)]
          (assoc @!state :retval retval))))))

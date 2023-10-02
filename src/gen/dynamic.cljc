(ns gen.dynamic
  (:require [clojure.walk :as walk]
            [gen.choice-map :as choice-map]
            [gen.dynamic.trace :as dynamic.trace]
            [gen.generative-function :as gf]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic :refer [untraced]])))

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

(defrecord DynamicDSLFunction [clojure-fn]
  gf/Simulate
  (simulate [gf args]
    (let [trace (atom (dynamic.trace/trace gf args))]
      (binding [dynamic.trace/*splice*
                (fn [gf args]
                  (let [subtrace (gf/simulate gf args)]
                    (swap! trace dynamic.trace/merge-subtraces subtrace)
                    (trace/retval subtrace)))

                dynamic.trace/*trace*
                (fn [k gf args]
                  (dynamic.trace/validate-empty! @trace k)
                  (let [subtrace (gf/simulate gf args)]
                    (swap! trace dynamic.trace/assoc-subtrace k subtrace)
                    (trace/retval subtrace)))]
        (let [retval (apply clojure-fn args)]
          (swap! trace dynamic.trace/with-retval retval)
          @trace))))

  gf/Generate
  (generate [gf args]
    (let [trace (gf/simulate gf args)]
      {:trace trace :weight (Math/log 1)}))
  (generate [gf args constraints]
    (let [state (atom {:trace (dynamic.trace/trace gf args)
                       :weight 0})]
      (binding [dynamic.trace/*splice*
                (fn [gf args]
                  (let [{subtrace :trace
                         weight :weight}
                        (gf/generate gf args constraints)]
                    (swap! state update :trace dynamic.trace/merge-subtraces subtrace)
                    (swap! state update :weight + weight)
                    (trace/retval subtrace)))

                dynamic.trace/*trace*
                (fn [k gf args]
                  (dynamic.trace/validate-empty! (:trace @state) k)
                  (let [{subtrace :trace :as ret}
                        (if-let [k-constraints (get (choice-map/submaps constraints) k)]
                          (gf/generate gf args k-constraints)
                          (gf/generate gf args))]
                    (swap! state dynamic.trace/combine k ret)
                    (trace/retval subtrace)))]
        (let [retval (apply clojure-fn args)
              trace (:trace @state)]
          {:trace (dynamic.trace/with-retval trace retval)
           :weight (:weight @state)}))))

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
        [params & body] (if name (rest xs) xs)]
    `(->DynamicDSLFunction
      (fn ~@(when name [name])
        ~params
        ~@(walk/postwalk
           (fn [form]
             (cond (trace-form? form)
                   (let [[addr gf & xs] (rest form)]
                     `((dynamic.trace/active-trace) ~addr ~gf [~@xs]))

                   (splice-form? form)
                   (let [[gf & xs] (rest form)]
                     `((dynamic.trace/active-splice) ~gf [~@xs]))

                   :else form))
           body)))))

(defmacro gen
  "Defines a generative function."
  [& args]
  {:clj-kondo/lint-as 'clojure.core/fn}
  (apply gen-body args))

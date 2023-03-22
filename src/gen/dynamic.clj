(ns gen.dynamic
  (:require [clojure.math :as math]
            [clojure.walk :as walk]
            [gen]
            [gen.choice-map :as choice-map]
            [gen.dynamic.trace :as dynamic.trace]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

(defrecord DynamicDSLFunction [clojure-fn]
  clojure.lang.IFn
  (invoke [_] (clojure-fn))
  (invoke [_ arg1] (clojure-fn arg1))
  (invoke [_ arg1 arg2] (clojure-fn arg1 arg2))
  (invoke [_ arg1 arg2 arg3] (clojure-fn arg1 arg2 arg3))
  (invoke [_ arg1 arg2 arg3 arg4] (clojure-fn arg1 arg2 arg3 arg4))
  (invoke [_ arg1 arg2 arg3 arg4 arg5] (clojure-fn arg1 arg2 arg3 arg4 arg5))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 s] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 s))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20] (clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20))
  (invoke [_ arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 args] (apply clojure-fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 args))
  (applyTo [_ arglist] (.applyTo clojure-fn arglist))

  gf/Simulate
  (simulate [gf args]
    (let [trace (atom (dynamic.trace/trace gf args))]
      (binding [dynamic.trace/*trace* (fn [k gf args]
                                        (let [subtrace (gf/simulate gf args)]
                                          (swap! trace dynamic.trace/assoc-subtrace k subtrace)
                                          (trace/retval subtrace)))]
        (let [retval (apply gf args)]
          (swap! trace dynamic.trace/set-retval! retval)
          @trace))))

  gf/Generate
  (generate [gf args]
    (let [trace (gf/simulate gf args)]
      {:trace trace :weight (math/log 1)}))
  (generate [gf args constraints]
    (let [state (atom {:trace (dynamic.trace/trace gf args)
                       :weight 0})]
      (binding [dynamic.trace/*trace*
                (fn [k gf args]
                  (let [{subtrace :trace
                         weight :weight}
                        (if-let [constraints (get (choice-map/submaps constraints)
                                                  k)]
                          (gf/generate gf args constraints)
                          (gf/generate gf args))]
                    (swap! state update :trace dynamic.trace/assoc-subtrace k subtrace)
                    (swap! state update :weight + weight)
                    (trace/retval subtrace)))]
        (let [retval (apply gf args)
              trace (:trace @state)]
          (dynamic.trace/set-retval! trace retval)
          {:trace trace
           :weight (:weight @state)})))))

(defn trace-form?
  "Returns true if `form` is a trace form."
  [form]
  (and (seq? form)
       (= `gen/trace (first form))))

(defn valid-trace-form?
  "Returns true if `form` is a valid trace form."
  [form]
  (and (trace-form? form)
       (>= (count form) 3)
       (let [call (nth form 2)]
         (and (seq? call)
              (let [gfn (first call)]
                (or (symbol? gfn)
                    (and (seq gfn)
                         (= `gen (first gfn)))))))))

(defmacro gen
  "Defines a generative function."
  [& args]
  (let [name (when (simple-symbol? (first args))
               (first args))
        [params & body] (if name (rest args) args)]
    `(->DynamicDSLFunction
      (fn ~@(when name [name])
        ~params
        ~@(walk/postwalk (fn [form]
                           (if-not (trace-form? form)
                             form
                             (if-not (valid-trace-form? form)
                               (throw (ex-info "Malformed trace expression." {:form form}))
                               (let [[addr [gf & args]] (rest form)]
                                 `(dynamic.trace/*trace* ~addr ~gf ~(vec args))))))
                         body)))))

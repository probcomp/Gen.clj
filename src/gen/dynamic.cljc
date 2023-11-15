(ns gen.dynamic
  (:require [clojure.pprint :as pprint]
            [clojure.walk :as walk]
            [gen.choicemap :as choicemap]
            [gen.diff :as diff]
            [gen.generative-function :as gf]
            [gen.trace :as trace])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap
                            MapEntry)))
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

;; ## trace impl

(defn no-op
  ([gf args]
   (apply gf args))
  ([_k gf args]
   (apply gf args)))

(def ^:dynamic *trace*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  no-op)

(defn active-trace
  "Returns the currently-active tracing function, bound to [[*trace*]].

  NOTE: Prefer `([[active-trace]])` to `[[*trace*]]`, as direct access to
  `[[*trace*]]` won't reflect new bindings when accessed inside of an SCI
  environment."
  [] *trace*)

;; TODO move `trace!` to `gen`.

(defmacro untraced
  [& body]
  `(binding [*trace* no-op]
     ~@body))

;; ## Choice Map for address-like trace

(defrecord Call [subtrace score noise])

(deftype ChoiceMap [m]
  choicemap/IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ k] (contains? m k))
  (get-submap [this k] (.invoke ^IFn this k choicemap/EMPTY))

  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (let [m (trace/get-choices (:subtrace v))]
          (if (choicemap/-has-value? m)
            (assoc! acc k (choicemap/-get-value m))
            acc)))
      (transient {})
      m)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (assoc! acc k (trace/get-choices (:subtrace v))))
      (transient {})
      m)))

  #?@(:clj
      [Object
       (equals [this that] (choicemap/equiv this that))
       (toString [this] (pr-str this))

       IFn
       (invoke [this k] (.invoke ^IFn this k nil))
       (invoke [_ k not-found]
               (if-let [v (get m k)]
                 (trace/get-choices (:subtrace v))
                 not-found))

       IObj
       (meta [_] (meta m))
       (withMeta [_ meta-m]
                 (ChoiceMap.
                  (with-meta m meta-m)))

       IPersistentMap
       (assocEx [_ _ _] (throw (Exception.)))
       (assoc [_ _ _]
              (throw
               (ex-info "ChoiceMap instances are read-only." {})))
       (without [m k]
                (ChoiceMap. (dissoc m k)))

       Associative
       (containsKey [_ k] (contains? m k))
       (entryAt [this k]
                (when (contains? m k)
                  (MapEntry/create k (.invoke ^IFn this k nil))))
       (cons [_ _]
             (throw
              (ex-info "ChoiceMap instances are read-only." {})))

       (count [_] (count m))
       (seq [_]
            (when-let [kvs (seq m)]
              (map (fn [[k v]]
                     (MapEntry/create k (trace/get-choices (:subtrace v))))
                   kvs)))

       (empty [_] choicemap/EMPTY)
       (valAt [this k] (.invoke ^IFn this k nil))
       (valAt [this k not-found] (.invoke ^IFn this k not-found))
       (equiv [this that] (choicemap/equiv this that))

       Iterable
       (iterator [this]
                 (.iterator ^Iterable (choicemap/get-submaps-shallow this)))]

      :cljs
      [Object
       (toString [_] (pr-str m))
       (equiv [this that] (choicemap/equiv this that))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-pr-writer m writer opts))

       IFn
       (-invoke [this k] (-invoke this k nil))
       (-invoke [_ k not-found]
                (if-let [v (get m k)]
                  (trace/get-choices (:subtrace v))
                  not-found))

       IMeta
       (-meta [_] (-meta m))

       IWithMeta
       (-with-meta [_ meta-m]
                   (ChoiceMap.
                    (-with-meta m meta-m)))

       IEmptyableCollection
       (-empty [_] choicemap/EMPTY)

       IEquiv
       (-equiv [this that] (choicemap/equiv this that))

       ISeqable
       (-seq [_] (-seq m))

       ICounted
       (-count [_] (-count m))

       ILookup
       (-lookup [_ k] (-invoke m k nil))
       (-lookup [_ k not-found] (-invoke m k not-found))

       IAssociative
       (-assoc [_ _ _]
               (throw
                (ex-info "ChoiceMap instances are read-only." {})))
       (-contains-key? [_ k] (-contains-key? m k))

       IMap
       (-dissoc [_ k]
                (ChoiceMap.
                 (dissoc m k)))]))

#?(:clj
   (defmethod print-method ChoiceMap
     [^ChoiceMap cm ^java.io.Writer w]
     (-> (choicemap/get-submaps-shallow cm)
         (print-method w))))

(defmethod pprint/simple-dispatch ChoiceMap [cm]
  (pprint/simple-dispatch
   (choicemap/get-submaps-shallow cm)))

(deftype Trace [gen-fn trie score noise args retval]
  trace/ITrace
  (get-args [_] args)
  (get-retval [_] retval)
  (get-gen-fn [_] gen-fn)
  (get-choices [_] (->ChoiceMap trie))
  (get-score [_] score))

#?(:clj
   (defmethod print-method Trace
     [^Trace t ^java.io.Writer w]
     (print-method (trace/trace->map t) w)))

(defmethod pprint/simple-dispatch Trace [^Trace t]
  (pprint/simple-dispatch (trace/trace->map t)))

(defn trace
  "Returns a new bare trace.

  TODO pad args with defaults if available."
  [gen-fn args]
  (Trace. gen-fn {} 0.0 0.0 args nil))

(defn validate-empty!
  [^Trace trace addr]
  (when (contains? (.-trie trace) addr)
    (throw
     (ex-info
      "Subtrace already present at address. The same address cannot be reused
      for multiple random choices."
      {:addr addr}))))

(defn with-retval [^Trace trace retval]
  (Trace. (.-gen-fn trace)
          (.-trie trace)
          (.-score trace)
          (.-noise trace)
          (.-args trace)
          retval))

(defn add-call
  "TODO handle noise."
  [^Trace trace k subtrace]
  (validate-empty! trace k)
  (let [trie (.-trie trace)
        score (trace/get-score subtrace)
        noise 0.0 #_ (trace/project subtrace nil)
        call  (->Call subtrace score noise)]
    (Trace. (.-gen-fn trace)
            (assoc trie k call)
            (+ (.-score trace) score)
            (+ (.-noise trace) noise)
            (.-args trace)
            (.-retval trace))))

(defn ^:no-doc trace:= [^Trace this that]
  (and (instance? Trace that)
       (let [^Trace that that]
         (and (= (.-gen-fn this) (.-gen-fn that))
              (= (.-trie this) (.-trie that))
              (= (.-score this) (.-score that))
              (= (.-noise this) (.-noise that))
              (= (.-args this) (.-args that))
              (= (.-retval this) (.-retval that))))))

;; ## Update State
(defn ^:no-doc combine
  "Combine trace update states. careful not to add "
  [v k {:keys [trace weight discard]}]
  {:trace   (add-call (:trace v) k trace)
   :weight  (+ (:weight v) weight)
   :discard (if (choicemap/empty? discard)
              (:discard v)
              (assoc (:discard v) k discard))})

;; ## Update impl
;;
;; TODO figure out what these notes mean!!

;; TODO this feels weird that we need something like this...
;;
;; TODO can we add exec to the protocol? NO but we can do `exec` if we move all
;; this nonsense into `gen.dynamic`... that would work!

(defn ^:no-doc extract-unvisited [^Trace prev-trace new-trace]
  (let [visited-m (choicemap/get-submaps-shallow
                   (trace/get-choices new-trace))
        unvisited-trie (apply dissoc
                              (.-trie prev-trace)
                              (keys visited-m))
        to-subtract (reduce-kv (fn [acc _ v] (+ acc (:score v)))
                               0.0
                               unvisited-trie)]

    [to-subtract (->ChoiceMap unvisited-trie)]))

(defn assert-all-visited! [^Trace trace constraints]
  (when-let [unvisited (keys
                        (apply dissoc
                               (choicemap/get-submaps-shallow constraints)
                               (keys (.-trie trace))))]
    (throw (ex-info "Some constraints weren't visited: "
                    {:unvisited unvisited}))))

(declare apply-inner)

(extend-type Trace
  trace/IUpdate
  (-update [this args _ constraints]
    (let [gen-fn (trace/get-gen-fn this)
          state  (atom {:trace   (trace gen-fn args)
                        :weight  0.0
                        :discard (choicemap/choicemap)})]
      (binding
          [*trace*
           (fn
             ([gf args]
              (apply-inner gf args))
             ([k gen-fn args]
              (validate-empty! (:trace @state) k)

              (let [k-constraints (choicemap/get-submap constraints k)
                    new-state
                    ;; TODO this is a spot where we'll need to check the
                    ;; previous value.
                    (if-let [prev-subtrace (:subtrace
                                            (get (.-trie this) k))]
                      (do
                        (assert
                         (= gen-fn (trace/get-gen-fn prev-subtrace))
                         (str "Generative function changed at address " k "."))
                        (trace/update prev-subtrace k-constraints))
                      (gf/generate gen-fn args k-constraints))]
                (swap! state combine k new-state)
                (trace/get-retval
                 (:trace new-state)))))]
        (let [retval                         (apply-inner gen-fn args)
              {:keys [trace weight discard]} @state
              [to-subtract unvisited]        (extract-unvisited this trace)]
          (assert-all-visited! trace constraints)
          {:trace   (with-retval trace retval)
           :change  diff/unknown-change
           :weight  (- weight to-subtract)
           :discard (choicemap/merge discard unvisited)})))))

;; so we are going to remove the score of the unvisited stuff as we go up. Does
;; that work?

(defrecord DynamicDSLFunction [clojure-fn has-argument-grads accepts-output-grad?]
  gf/IGenerativeFunction
  (has-argument-grads [_] has-argument-grads)
  (accepts-output-grad? [_] accepts-output-grad?)
  (get-params [_] ())
  (simulate [gf args]
    (let [!trace (atom (trace gf args))]
      (binding [*trace*
                (fn
                  ([gf args]
                   (apply-inner gf args))
                  ([k gf args]
                   (validate-empty! @!trace k)
                   (let [subtrace (gf/simulate gf args)]
                     (swap! !trace add-call k subtrace)
                     (trace/get-retval subtrace))))]
        (let [retval (apply clojure-fn args)
              trace  @!trace]
          (with-retval trace retval)))))

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

(defn ^:no-doc apply-inner [^DynamicDSLFunction gf args]
  (apply (.-clojure-fn gf) args))

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
                        `((active-trace) ~addr ~gf [~@xs]))

                      (splice-form? form)
                      (let [[gf & xs] (rest form)]
                        `((active-trace) ~gf [~@xs]))

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
      (update :trace add-call k trace)
      (update :weight + weight)))

;; TODO figure out visited / unvisited??

(extend-type DynamicDSLFunction
  gf/IGenerate
  (-generate [gf args constraints]
    (let [trace  (trace gf args)
          !state (atom {:trace trace :weight 0.0})]
      (binding [*trace*
                (fn
                  ([gf args]
                   (apply-inner gf args))
                  ([k gf args]
                   (validate-empty! (:trace @!state) k)
                   (let [{subtrace :trace :as ret}
                         (let [k-constraints (choicemap/get-submap constraints k)]
                           (gf/generate gf args k-constraints))]
                     (swap! !state assoc-state k ret)
                     (trace/get-retval subtrace))))]
        (let [retval (apply-inner gf args)
              state  @!state]
          (update state :trace with-retval retval)))))

  gf/IAssess
  (-assess [gf args choices]
    (let [!weight (atom 0.0)]
      (binding [*trace*
                (fn
                  ([gf args]
                   (apply-inner gf args))
                  ([k gf args]
                   (let [{:keys [weight retval]}
                         (let [k-choices (choicemap/get-submap choices k)]
                           (gf/assess gf args k-choices))]
                     (swap! !weight + weight)
                     retval)))]
        (let [retval (apply-inner gf args)]
          {:weight @!weight
           :retval retval}))))

  gf/IPropose
  (propose [gf args]
    (let [!state (atom {:choices (choicemap/choicemap)
                        :weight  0.0})]
      (binding [*trace*
                (fn
                  ([gf args]
                   (apply-inner gf args))
                  ([k gf args]
                   (let [{:keys [submap weight retval]} (gf/propose gf args)]
                     (swap! !state
                            (fn [m]
                              (-> m
                                  (update :choices assoc k submap)
                                  (update :weight + weight))))
                     retval)))]
        (let [retval (apply-inner gf args)]
          (assoc @!state :retval retval))))))

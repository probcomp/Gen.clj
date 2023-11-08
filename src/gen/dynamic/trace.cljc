(ns gen.dynamic.trace
  (:refer-clojure :exclude [=])
  (:require [clojure.core :as core]
            [gen.diff :as diff]
            [gen.choice-map :as cm]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic.trace]))
  #?(:clj
     (:import
      (clojure.lang Associative IFn IObj IMapIterable))))

(defn no-op
  ([gf args]
   (apply gf args))
  ([_k gf args]
   (apply gf args)))

(def ^:dynamic *trace*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  no-op)

(def ^:dynamic *splice*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  no-op)

(defn active-trace
  "Returns the currently-active tracing function, bound to [[*trace*]].

  NOTE: Prefer `([[active-trace]])` to `[[*trace*]]`, as direct access to
  `[[*trace*]]` won't reflect new bindings when accessed inside of an SCI
  environment."
  [] *trace*)

(defn active-splice
  "Returns the currently-active tracing function, bound to [[*splice*]].

  NOTE: Prefer `([[active-splice]])` to `[[*splice*]]`, as direct access to
  `[[*splice*]]` won't reflect new bindings when accessed inside of an SCI
  environment."
  []
  *splice*)

(declare trace =)

;; wrapper type!

(defrecord Choice [retval score]
  cm/IArray
  (to-array [_] [retval])
  (-from-array [_ xs idx]
    [1 (Choice. (nth xs idx) score)]))

;; TODO I THINK I WILL ONLY NEED THIS and can get rid of the Choice abstraction.
(defrecord Call [subtrace score noise])

(deftype ChoiceMap [m]
  cm/IChoiceMap
  (has-value? [m k]
    (instance? Choice (get m k)))

  (get-value [m k]
    (when-let [v (get m k)]
      (when (instance? Choice v)
        (:retval v))))

  (has-submap? [m k]
    (instance? Call (get m k)))

  ;; TODO these error on the wrong fetch in the original. is that right?? and
  ;; empty returns EmptyChoiceMap, never nil.
  (get-submap [m k]
    (when-let [v (get m k)]
      (cond (instance? Call v)
            (trace/get-choices (:subtrace v))

            (map? v)
            (ChoiceMap. v)

            :else nil)))

  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (if (instance? Choice v)
          (assoc! acc k (:retval v))
          acc))
      (transient {})
      m)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (cond (instance? Call v)
              (assoc! acc k (trace/get-choices (:subtrace v)))

              (map? v)
              (assoc! acc k (ChoiceMap. v))

              :else acc))
      (transient {})
      m))))

(deftype Trace [gen-fn trie score noise args retval]
  trace/ITrace
  (get-args [_] args)
  (get-retval [_] retval)
  (get-gen-fn [_] gen-fn)
  (get-choices [_] (->ChoiceMap trie))
  (get-score [_] score)

  #?@(:cljs
      [Object
       (equiv [this other] (-equiv this other))

       IFn
       (-invoke [this k] (-lookup this k))
       (-invoke [this k not-found] (-lookup this k not-found))

       IMeta
       (-meta [_] (meta trie))

       IWithMeta
       (-with-meta [_ m] (Trace. gen-fn args (with-meta trie m) retval))


       ;; ICloneable
       ;; (-clone [_] (Trace. (-clone m)))

       IIterable
       (-iterator [this] (-iterator (trace/get-choices this)))

       ;; ICollection
       ;; (-conj [_ entry])

       ;; IEmptyableCollection
       ;; (-empty [_])

       IEquiv
       (-equiv [this that] (= this that))

       ;; IHash
       ;; (-hash [_] (-hash m))

       ISeqable
       (-seq [this] (-seq (trace/get-choices this)))

       ICounted
       (-count [_] (-count trie))

       ILookup
       (-lookup [this k]
                (-lookup (trace/get-choices this) k))
       (-lookup [this k not-found]
                (-lookup (trace/get-choices this) k not-found))

       IAssociative
       ;; (-assoc [_ k v] (Trace. (-assoc m k (choice v))))
       (-contains-key? [_ k] (-contains-key? trie k))

       IFind
       (-find [this k]
              (-find (trace/get-choices this) k))]

      :clj
      [Object
       (equals [this that] (= this that))

       IFn
       (invoke [this k] (.valAt this k))
       (invoke [this k not-found] (.valAt this k not-found))

       IObj
       (meta [_] (meta trie))
       (withMeta [_ m] (Trace. gen-fn (with-meta trie m) score noise args retval))

       Associative
       (containsKey [_ k] (contains? trie k))
       (entryAt [_ k] (.entryAt ^Associative trie k))
       (count [_] (count trie))
       (seq [this] (seq (trace/get-choices this)))
       (valAt [this k]
              (get (trace/get-choices this) k))
       (valAt [this k not-found]
              (get (trace/get-choices this) k not-found))
       (equiv [this that] (= this that))
       ;; TODO missing `cons`, `empty`?

       IMapIterable
       (keyIterator [this]
                    (.iterator ^Iterable (keys (trace/get-choices this))))
       (valIterator [this]
                    (.iterator ^Iterable (vals (trace/get-choices this))))

       Iterable
       (iterator [this]
                 (.iterator ^Iterable (trace/get-choices this)))]))

(defn trace
  "new trace.

  TODO pad args with defaults if available."
  [gen-fn args]
  (Trace. gen-fn {} 0.0 0.0 args nil))

(defn validate-empty! [m addr]
  (when (contains? m addr)
    (throw (ex-info "Value or subtrace already present at address. The same
                      address cannot be reused for multiple random choices."
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
  (let [trie (.-trie trace)]
    (validate-empty! trie k)
    (let [score (trace/get-score subtrace)
          noise 0.0 #_ (trace/project subtrace nil)
          call  (->Call subtrace score noise)]
      (Trace. (.-gen-fn trace)
              (assoc trie k call)
              (+ (.-score trace) score)
              (+ (.-noise trace) noise)
              (.-args trace)
              (.-retval trace)))))

(defn ^:no-doc = [^Trace this that]
  (and (instance? Trace that)
       (let [^Trace that that]
         (and (core/= (.-gen-fn this) (.-gen-fn that))
              (core/= (.-trie this) (.-trie that))
              (core/= (.-score this) (.-score that))
              (core/= (.-noise this) (.-noise that))
              (core/= (.-args this) (.-args that))
              (core/= (.-retval this) (.-retval that))))))


;; ## Update State
(defn ^:no-doc combine
  "combine by adding weights?"
  [v k {:keys [trace weight discard]}]
  (-> v
      (update :trace add-call k trace)
      (update :weight + weight)
      (cond-> discard (update :discard assoc k discard))))

;; ## Update impl
(extend-type Trace
  trace/IUpdate
  (-update [this _ _ constraints]
    ;; TODO this feels weird that we need something like this...
    ;;
    ;; TODO can we add exec to the protocol?
    (let [^gen.dynamic.DynamicDSLFunction gen-fn (trace/get-gen-fn this)
          state (atom {:trace (trace gen-fn (trace/get-args this))
                       :weight 0
                       :discard (dynamic.choice-map/choice-map)})]
      (binding [*splice*
                (fn [_ _args]
                  (throw (ex-info "Not yet implemented." {})))

                *trace*
                ;; TODO this needs a major redo to match the interface.
                (fn [k gen-fn args]
                  (validate-empty! (:trace @state) k)
                  (let [k-constraints (cm/get-submap constraints k)
                        {subtrace :trace :as ret}
                        (if-let [prev-subtrace (get (.-trie this) k)]
                          (trace/update prev-subtrace k-constraints)
                          (gf/generate gen-fn args k-constraints))]
                    (swap! state combine k ret)
                    (trace/get-retval subtrace)))]
        (let [retval (apply (.-clojure-fn gen-fn) (trace/get-args this))
              {:keys [trace weight discard]} @state
              unvisited (apply dissoc
                               (trace/get-choices this)
                               (keys (trace/get-choices trace)))]

          {:trace (with-retval trace retval)
           :change diff/unknown-change
           :weight weight
           :discard (merge discard unvisited)})))))

;; ## Primitive Trace
;;
;; [[Trace]] above tracks map-like associations of address to traced value. At
;; the bottom of the tree represented by these associations is a primitive
;; trace, usually generated by a primitive probability distribution.
;;
;; [[PrimitiveTrace]] is a simplified version of [[Trace]] (and an implementer
;; of the [[gen.trace]] interface) designed for a single value.

(defrecord PrimitiveTrace [gen-fn args val score]
  trace/ITrace
  (get-args [_] args)
  (get-retval [_] val)
  ;; TODO yes, this is the reason we need ValueChoiceMap, a single thing...
  ;; fix!!
  (get-choices [_] val)
  (get-gen-fn [_] gen-fn)
  (get-score [_] score)

  trace/IUpdate
  (-update [_ _ _ constraint]
    (-> (gf/generate gen-fn args constraint)
        (update :weight - score)
        (core/assoc :change  diff/unknown-change
                    :discard val))))

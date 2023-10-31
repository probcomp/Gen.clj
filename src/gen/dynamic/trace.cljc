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

(declare assoc-subtrace update-trace trace =)

(deftype Trace [gf args subtraces retval]
  trace/Args
  (args [_]
    args)

  trace/Choices
  (choices [_]
    (let [m (update-vals subtraces trace/choices)]
      (dynamic.choice-map/->ChoiceMap m (into [] (keys m)))))

  trace/GenFn
  (gf [_]
    gf)

  trace/RetVal
  (retval [_] retval)

  trace/Score
  (score [_]
    ;; TODO Handle untraced randomness.
    (let [v (vals subtraces)]
      (transduce (map trace/score) + 0.0 v)))

  trace/Update
  (update [this constraints]
    (update-trace this constraints))

  #?@(:cljs
      [Object
       (equiv [this other] (-equiv this other))

       IFn
       (-invoke [this k] (-lookup this k))
       (-invoke [this k not-found] (-lookup this k not-found))

       IMeta
       (-meta [_] (meta subtraces))

       IWithMeta
       (-with-meta [_ m] (Trace. gf args (with-meta subtraces m) retval))


       ;; ICloneable
       ;; (-clone [_] (Trace. (-clone m)))

       IIterable
       (-iterator [this] (-iterator (trace/choices this)))

       ;; ICollection
       ;; (-conj [_ entry])

       ;; IEmptyableCollection
       ;; (-empty [_])

       IEquiv
       (-equiv [this that] (= this that))

       ;; IHash
       ;; (-hash [_] (-hash m))

       ISeqable
       (-seq [this] (-seq (trace/choices this)))

       ICounted
       (-count [_] (-count subtraces))

       ILookup
       (-lookup [this k]
                (-lookup (trace/choices this) k))
       (-lookup [this k not-found]
                (-lookup (trace/choices this) k not-found))

       IAssociative
       ;; (-assoc [_ k v] (Trace. (-assoc m k (choice v))))
       (-contains-key? [_ k] (-contains-key? subtraces k))

       IFind
       (-find [this k]
              (-find (trace/choices this) k))]

      :clj
      [Object
       (equals [this that] (= this that))

       IFn
       (invoke [this k] (.valAt this k))
       (invoke [this k not-found] (.valAt this k not-found))

       IObj
       (meta [_] (meta subtraces))
       (withMeta [_ m] (Trace. gf args (with-meta subtraces m) retval))

       Associative
       (containsKey [_ k] (contains? subtraces k))
       (entryAt [_ k] (.entryAt ^Associative subtraces k))
       (count [_] (count subtraces))
       (seq [this] (seq (trace/choices this)))
       (valAt [this k]
              (get (trace/choices this) k))
       (valAt [this k not-found]
              (get (trace/choices this) k not-found))
       (equiv [this that] (= this that))
       ;; TODO missing `cons`, `empty`?

       IMapIterable
       (keyIterator [this]
                    (.iterator ^Iterable (keys (trace/choices this))))
       (valIterator [this]
                    (.iterator ^Iterable (vals (trace/choices this))))

       Iterable
       (iterator [this]
                 (.iterator ^Iterable (trace/choices this)))]))

(defn ^:no-doc = [^Trace this that]
  (and (instance? Trace that)
       (let [^Trace that that]
         (and (core/= (.-gf this) (.-gf that))
              (core/= (.-args this) (.-args that))
              (core/= (.-subtraces this) (.-subtraces that))
              (core/= (.-retval this) (.-retval that))))))

(defn trace
  [gf args]
  (->Trace gf args {} nil))

(defn with-retval [^Trace t v]
  (->Trace (.-gf t) (.-args t) (.-subtraces t) v))

(defn validate-empty! [t addr]
  (when (contains? t addr)
    (throw (ex-info "Value or subtrace already present at address. The same
                      address cannot be reused for multiple random choices."
                    {:addr addr}))))

(defn assoc-subtrace
  [^Trace t addr subt]
  (validate-empty! t addr)
  (->Trace (.-gf t)
             (.-args t)
             (assoc (.-subtraces t) addr subt)
             (.-retval t)))

(defn merge-subtraces
  [^Trace t1 ^Trace t2]
  (reduce-kv assoc-subtrace
             t1
             (.-subtraces t2)))

(defn ^:no-doc combine
  "combine by adding weights?"
  [v k {:keys [trace weight discard]}]
  (-> v
      (update :trace assoc-subtrace k trace)
      (update :weight + weight)
      (cond-> discard (update :discard assoc k discard))))

(defn update-trace [^Trace this constraints]
  (let [gf (trace/gf this)
        state (atom {:trace (trace gf (trace/args this))
                     :weight 0
                     :discard (dynamic.choice-map/choice-map)})]
    (binding [*splice*
              (fn [& _]
                (throw (ex-info "Not yet implemented." {})))

              *trace*
              (fn [k gf args]
                (validate-empty! (:trace @state) k)
                (let [k-constraints (get (cm/get-submaps-shallow constraints) k)
                      {subtrace :trace :as ret}
                      (if-let [prev-subtrace (get (.-subtraces this) k)]
                        (trace/update prev-subtrace k-constraints)
                        (gf/generate gf args k-constraints))]
                  (swap! state combine k ret)
                  (trace/retval subtrace)))]
      (let [retval (apply (:clojure-fn gf) (trace/args this))
            {:keys [trace weight discard]} @state
            unvisited (apply dissoc
                             (trace/choices this)
                             (keys (trace/choices trace)))]

        {:trace (with-retval trace retval)
         :weight weight
         :discard (merge discard unvisited)}))))

;; ## Primitive Trace
;;
;; [[Trace]] above tracks map-like associations of address to traced value. At
;; the bottom of the tree represented by these associations is a primitive
;; trace, usually generated by a primitive probability distribution.
;;
;; [[PrimitiveTrace]] is a simplified version of [[Trace]] (and an implementer
;; of the [[gen.trace]] interface) designed for a single value.

(declare update-primitive)

(defrecord PrimitiveTrace [gf args val score]
  trace/GenFn
  (gf [_] gf)

  trace/Args
  (args [_] args)

  trace/RetVal
  (retval [_] val)

  trace/Choices
  (choices [_] (dynamic.choice-map/choice val))

  trace/Score
  (score [_] score)

  trace/Update
  (update [trace constraint]
    (update-primitive trace constraint))
  (update [_ _ _ _]
    (throw
     (ex-info "Not yet implemented for primitive distributions." {}))))

(defn ^:no-doc update-primitive
  "Accepts a [[PrimitiveTrace]] instance `t` and a
  single [[gen.dynamic.choice-map/Choice]] and returns a new object with keys
  `:trace`, `:weight` and `:change`."
  [t constraint]
  {:pre [(instance? PrimitiveTrace t)]}
  (cond (dynamic.choice-map/choice? constraint)
        (-> (trace/gf t)
            (gf/generate (trace/args t) constraint)
            (update :weight - (trace/score t))
            (core/assoc :change  diff/unknown-change
                        :discard (trace/choices t)))

        (nil? constraint)
        {:trace t
         :weight 0.0
         :change diff/unknown-change}

        (map? constraint)
        (throw
         (ex-info
          "Expected a value at address but found a sub-assignment."
          {:sub-assignment constraint}))

        :else
        (throw
         (ex-info
          "non-nil, non-Choice constraint not allowed."
          {:sub-assignment constraint}))))

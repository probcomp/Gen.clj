(ns gen.dynamic.trace
  (:refer-clojure :exclude [assoc])
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [gen.choice-map :as cm]
            [gen.diff :as diff]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

(defn ^:no-doc no-op
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

(defmacro untraced
  [& body]
  `(binding [*trace*  no-op
             *splice* no-op]
     ~@body))

(declare assoc trace update-trace with-val compute-score)

(deftype Trace [gf args subtraces retval]
  clojure.lang.Associative
  (containsKey [_ k]
    (contains? subtraces k))
  (entryAt [_ k]
    (.entryAt subtraces k))

  clojure.lang.Counted
  (count [_]
    (count subtraces))

  clojure.lang.IFn
  (invoke [this k]
    (.valAt this k))

  clojure.lang.ILookup
  (valAt [this k]
    (get (trace/choices this) k))
  (valAt [this k not-found]
    (get (trace/choices this) k not-found))

  clojure.lang.IMapIterable
  (keyIterator [this]
    (.iterator
     ^java.lang.Iterable
     (keys (trace/choices this))))
  (valIterator [this]
    (.iterator
     ^java.lang.Iterable
     (map cm/unwrap (vals (trace/choices this)))))

  clojure.lang.Seqable
  (seq [this]
    (seq (trace/choices this)))

  java.lang.Iterable
  (iterator [this]
    (.iterator
     (let [^clojure.lang.Seqable choice-map (trace/choices this)]
       ^java.lang.Iterable (.seq choice-map))))

  trace/GenFn
  (gf [_] gf)

  trace/Args
  (args [_] args)

  trace/RetVal
  (retval [_] retval)

  trace/Choices
  (choices [_]
    ;; NOTE if you call `choices` on a trace you do NOT get a map of
    ;; constraints! you get a map of choices.
    (cm/unwrap
     (update-vals subtraces trace/choices)))

  trace/Score
  (score [_]
    ;; TODO Handle untraced randomness.
    (let [v (vals subtraces)]
      (transduce (map trace/score) + 0.0 v)))

  trace/Update
  (update [this constraints]
    (update-trace this constraints))
  (update [_ _ _ _]
    (throw
     (ex-info "Not yet implemented." {}))))

(defn update-primitive-trace
  "Updates a trace representing a primitive distribution."
  [t constraints]
  (cond (cm/choice? constraints)
        (-> (trace/gf t)
            (gf/generate (trace/args t) constraints)
            (update :weight - (trace/score t))
            (core/assoc :change    diff/unknown-change
                        :discard   (cm/choice
                                    (trace/retval t))))

        (nil? constraints)
        {:trace t
         :weight 0.0
         :change diff/unknown-change}

        (map? constraints)
        (throw
         (ex-info
          "Expected a value at address but found a sub-assignment."
          {:sub-assignment constraints}))

        :else
        (throw
         (ex-info
          "non-nil, non-Choice constraint not allowed."
          {:sub-assignment constraints}))))

;; TODO attempt at a leaf trace.

(defrecord PrimitiveTrace [gf args retval choices score]
  trace/GenFn
  (gf [_] gf)

  trace/Args
  (args [_] args)

  trace/RetVal
  (retval [_] retval)

  trace/Choices
  (choices [_] choices)

  trace/Score
  (score [_] score)

  trace/Update
  (update [trace constraints]
    (update-primitive-trace trace constraints))
  (update [_ _ _ _]
    (throw
     (ex-info "Not yet implemented for primitive distributions." {}))))

;; ## Constructors

(defn trace
  "Returns a new, unfulfilled [[Trace]] wrapping the supplied generative function
  and its arguments."
  [gf args]
  (->Trace gf args {} nil))

;; ## Predicates

(defn trace?
  "Returns `true` if `t` is an instance of [[Trace]], false otherwise.

  TODO change to a protocol based 'do you implement?'"
  [t]
  (instance? Trace t))

;; ## Functions

(defn with-val [^Trace t v]
  (->Trace (.-gf t) (.-args t) (.-subtraces t) v))

(defn ^:no-doc combine
  "combine by adding weights?"
  [l k {:keys [trace weight discard] :as r}]
  (-> l
      (update :trace assoc k trace)
      (update :weight + weight)
      (cond-> (contains? r :discard) (update :discard core/assoc k discard))))

(defn update-trace [this constraints]
  (let [gf    (trace/gf this)
        state (atom {:trace (trace gf (trace/args this))
                     :weight 0
                     :discard {}})]
    (binding [*splice*
              (fn [_gf _args]
                (throw (ex-info "Not yet implemented." {})))

              *trace*
              (fn [k gf args]
                (let [k-constraints (get constraints k)
                      ret (if-let [prev-subtrace (get (.-subtraces ^Trace this) k)]
                            ;; TODO why don't we use `gf` or `args`?
                            (trace/update prev-subtrace k-constraints)
                            (gf/generate gf args k-constraints))]
                  (swap! state combine k ret)
                  (trace/retval (:trace ret))))]
      (let [retval (apply (:clojure-fn gf) (trace/args this))
            {:keys [trace weight discard]} @state
            unvisited (select-keys
                       (trace/choices this)
                       (set/difference
                        (set (keys (trace/choices this)))
                        (set (keys (trace/choices trace)))))]
        {:trace (with-val trace retval)
         :weight  weight
         :discard (merge discard unvisited)}))))

(defn validate-empty! [t addr]
  (when (contains? t addr)
    (throw (ex-info "Value or subtrace already present at address. The same
                      address cannot be reused for multiple random choices."
                    {:addr addr}))))

(defn assoc
  [^Trace t addr subt]
  (validate-empty! t addr)
  (->Trace (.-gf t)
           (.-args t)
           (core/assoc (.-subtraces t) addr subt)
           (.-retval t)))

(defn merge-subtraces
  [^Trace t1 ^Trace t2]
  (reduce-kv assoc t1 (.-subtraces t2)))

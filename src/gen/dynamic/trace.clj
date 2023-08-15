(ns gen.dynamic.trace
  (:refer-clojure :exclude [=])
  (:require [clojure.core :as core]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.trace :as trace])
  (:import
   (clojure.lang Associative IFn IObj IMapIterable Seqable)))

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

(defmacro without-tracing
  [& body]
  `(binding [*trace* no-op
             *splice* no-op]
     ~@body))

(declare assoc-subtrace merge-trace set-retval! trace =)

(deftype Trace [gf args subtraces retval score]
  trace/Args
  (args [_]
    args)

  trace/Choices
  (choices [_]
    (dynamic.choice-map/->ChoiceMap (update-vals subtraces trace/choices)))

  trace/GenFn
  (gf [_]
    gf)

  trace/RetVal
  (retval [_]
    @retval)

  trace/Score
  (score [_]
    ;; TODO Handle untraced randomness.
    (if (realized? score)
      @score
      (do (deliver score (transduce (map trace/score) + 0 (vals subtraces)))
          @score)))

  Object
  (equals [this that] (= this that))
  (toString [this] (pr-str this))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  IObj
  (meta [_] (meta subtraces))
  (withMeta [_ m] (Trace. gf args (with-meta subtraces m) retval score))

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

  java.lang.Iterable
  (iterator [this]
    (.iterator
     (let [^Seqable choice-map (trace/choices this)]
       ^Iterable (.seq choice-map)))))

(defn ^:no-doc = [^Trace this that]
  (and (instance? Trace that)
       (let [^Trace that that]
         (and (core/= (.-gf this) (.-gf that))
              (core/= (.-args this) (.-args that))
              (core/= (.-subtraces this) (.-subtraces that))
              (core/= (.-retval this) (.-retval that))
              (core/= (.-score this) (.-score that))))))

(defn trace
  [gf args]
  (Trace. gf args {} (promise) (promise)))

(defn set-retval!
  [^Trace t retval]
  (deliver (.-retval t) retval)
  t)

(defn assoc-subtrace
  [^Trace t addr subt]
  (let [subtraces (.-subtraces t)]
    (when (contains? subtraces addr)
      (throw (ex-info "Value or subtrace already present at address. The same address cannot be reused for multiple random choices."
                      {:addr addr})))
    (Trace. (.-gf t)
            (.-args t)
            (assoc subtraces addr subt)
            (.-retval t)
            (.-score t))))

(defn merge-subtraces
  [^Trace t1 ^Trace t2]
  (reduce-kv assoc-subtrace
             t1
             (.-subtraces t2)))

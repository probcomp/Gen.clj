(ns gen.dynamic.trace
  (:refer-clojure :exclude #?(:clj  [deliver promise realized?]
                              :cljs [realized?]))
  (:require [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic.trace]))
  #?(:clj
     (:import
      (clojure.lang Associative IFn IObj IMapIterable Seqable))))

(def ^:no-doc promise
  #?(:cljs #(atom nil)
     :clj  clojure.core/promise))

(def ^:no-doc realized?
  #?(:cljs (fn [p] (some? @p))
     :clj  clojure.core/realized?))

(def ^:no-doc deliver
  #?(:cljs reset!
     :clj  clojure.core/deliver))

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

(declare t:=)

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

  #?@(:cljs
      [Object
       (toString [this] (pr-str this))
       (equiv [this other] (-equiv this other))

       IFn
       (-invoke [this k] (-lookup this k))
       (-invoke [this k not-found] (-lookup this k not-found))

       IMeta
       (-meta [_] (meta subtraces))

       IWithMeta
       (-with-meta [_ m] (Trace. gf args (with-meta subtraces m) retval score))


       ;; ICloneable
       ;; (-clone [_] (Trace. (-clone m)))

       IIterable
       (-iterator [this] (-iterator (trace/choices this)))

       ;; ICollection
       ;; (-conj [_ entry])

       ;; IEmptyableCollection
       ;; (-empty [_])

       IEquiv
       (-equiv [this that] (t:= this that))

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
       (equals [this that] (t:= this that))
       (toString [this] (pr-str this))

       IFn
       (invoke [this k] (.valAt this k))

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
       (equiv [this that] (t:= this that))
       ;; TODO missing `cons`, `empty`


       IMapIterable
       (keyIterator [this]
                    (.iterator
                     ^java.lang.Iterable
                     (keys (trace/choices this))))
       (valIterator [this]
                    (.iterator
                     ^java.lang.Iterable
                     (vals (trace/choices this))))

       java.lang.Iterable
       (iterator [this]
                 (.iterator
                  (let [^Seqable choice-map (trace/choices this)]
                    ^Iterable (.seq choice-map))))]))

(defn ^:no-doc t:= [^Trace this that]
  (and (instance? Trace that)
       (let [^Trace that that]
         (and (= (.-gf this) (.-gf that))
              (= (.-args this) (.-args that))
              (= (.-subtraces this) (.-subtraces that))
              (= (.-retval this) (.-retval that))
              (= (.-score this) (.-score that))))))

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
      (throw
       (ex-info "Value or subtrace already present at address. The same
                      address cannot be reused for multiple random choices."
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

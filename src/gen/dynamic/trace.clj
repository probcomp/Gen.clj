(ns gen.dynamic.trace
  (:require [clojure.set :as set]
            [gen.choice-map :as choice-map]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

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
  `(binding [*trace*  no-op
             *splice* no-op]
     ~@body))

(declare assoc-subtrace merge-trace set-retval! trace update-trace)

(deftype Trace [gf args ^clojure.lang.PersistentArrayMap subtraces retval score]
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
     (vals (trace/choices this))))

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
  (retval [_] @retval)

  trace/Choices
  (choices [_]
    (dynamic.choice-map/->ChoiceMap (update-vals subtraces trace/choices)))

  trace/Score
  (score [_]
    ;; TODO Handle untraced randomness.
    (if (realized? score)
      @score
      (do (deliver score (transduce (map trace/score)
                                    +
                                    0
                                    (vals subtraces)))
          @score)))

  trace/Update
  (update [this constraints]
    (update-trace this constraints)))

(defn update-trace [this constraints]
  (let [gf    (trace/gf this)
        state (atom {:trace (trace gf (trace/args this))
                     :weight 0
                     :discard (dynamic.choice-map/choice-map)})]
    (binding [*splice*
              (fn [k gf args]
                (let [{subtrace :trace
                       weight :weight
                       discard :discard}
                      (if-let [prev-subtrace (get (.-subtraces ^Trace this) k)]
                        (let [{new-subtrace :trace
                               new-weight :weight
                               discard :discard}
                              (trace/update prev-subtrace
                                            (get (choice-map/submaps constraints)
                                                 k))]
                          {:trace new-subtrace
                           :weight new-weight
                           :discard discard})
                        (gf/generate gf args (get (choice-map/submaps constraints) k)))]
                  (swap! state
                         (fn [s]
                           (-> s
                               (update :trace merge subtrace)
                               (update :weight + weight)
                               (cond->  discard (update :discard assoc k discard)))))
                  (trace/retval subtrace)))

              *trace*
              (fn [k gf args]
                (let [{subtrace :trace
                       weight :weight
                       discard :discard}
                      (if-let [prev-subtrace (get (.-subtraces ^Trace this) k)]
                        (let [{new-subtrace :trace
                               new-weight :weight
                               discard :discard}
                              (trace/update prev-subtrace
                                            (get (choice-map/submaps constraints)
                                                 k))]
                          {:trace new-subtrace
                           :weight new-weight
                           :discard discard})
                        (gf/generate gf args (get (choice-map/submaps constraints)
                                                  k)))]
                  (swap! state update :trace assoc-subtrace k subtrace)
                  (swap! state update :weight + weight)
                  (when discard
                    (swap! state update :discard assoc k discard))
                  (trace/retval subtrace)))]
      (let [retval (apply (:clojure-fn gf) (trace/args this))
            {:keys [trace weight discard]} @state
            unvisited (select-keys
                       (trace/choices this)
                       (set/difference
                        (set (keys (trace/choices this)))
                        (set (keys (trace/choices trace)))))]
        (set-retval! trace retval)
        {:trace trace
         :weight weight
         :discard (merge discard unvisited)}))))

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
      (throw (ex-info "Value or subtrace already present at address. The same
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

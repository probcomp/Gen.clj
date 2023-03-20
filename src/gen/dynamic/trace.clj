(ns gen.dynamic.trace
  (:require [clojure.core :as core]
            [clojure.set :as set]
            [gen.choice-map :as choice-map]
            [gen.dynamic.choice-map :as dynamic.choice-map]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

(defn ^:dynamic *trace*
  "Applies the generative function gf to args. Dynamically rebound by functions
  like `gf/simulate`, `gf/generate`, `trace/update`, etc."
  [_k gf args]
  (apply gf args))

(declare assoc-subtrace set-retval! trace)

(deftype Trace [gf args subtraces retval score]
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
     ^java.lang.Iterable
     (.seq (trace/choices this))))

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
      (do (deliver score (transduce (map trace/score)
                                    +
                                    0
                                    (vals subtraces)))
          @score)))

  trace/Update
  (update [prev-trace constraints]
    (let [state (atom {:trace (trace gf (trace/args prev-trace))
                       :weight 0
                       :discard (dynamic.choice-map/choice-map)})]
      (binding [*trace* (fn [k gf args]
                          (let [{subtrace :trace
                                 weight :weight
                                 discard :discard}
                                (if-let [prev-subtrace (get (.-subtraces prev-trace) k)]
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
        (let [retval (apply gf (trace/args prev-trace))
              {:keys [trace weight discard]} @state
              unvisited (select-keys (trace/choices prev-trace)
                                     (set/difference (set (keys (trace/choices prev-trace)))
                                                     (set (keys (trace/choices trace)))))]
          (set-retval! trace retval)
          {:trace trace
           :weight weight
           :discard (merge discard unvisited)})))))

(defn trace
  [gf args]
  (Trace. gf args {} (promise) (promise)))

(defn set-retval!
  [t retval]
  (deliver (.-retval t) retval)
  t)

(defn assoc-subtrace
  [t addr subt]
  (let [subtraces (.-subtraces t)]
    (when (contains? subtraces addr)
      (throw (ex-info "Value or subtrace already present at address. The same address cannot be reused for multiple random choices." {})))
    (Trace. (.-gf t)
            (.-args t)
            (assoc subtraces addr subt)
            (.-retval t)
            (.-score t))))

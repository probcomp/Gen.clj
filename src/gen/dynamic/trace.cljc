(ns gen.dynamic.trace
  "TODO move this nonsense over to gen.dynamic??"
  (:refer-clojure :exclude [=])
  (:require [clojure.core :as core]
            [gen.diff :as diff]
            [gen.choicemap :as choicemap]
            [gen.generative-function :as gf]
            [gen.trace :as trace])
  #?(:cljs
     (:require-macros [gen.dynamic.trace])))

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

(declare =)

;; ## Choice Map for address-like trace

(defrecord Call [subtrace score noise])

(deftype ChoiceMap [m]
  choicemap/IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [m k] (contains? m k))
  (get-submap [m k]
    (if-let [v (get m k)]
      (trace/get-choices (:subtrace v))
      choicemap/EMPTY))

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
      m))))

(deftype Trace [gen-fn trie score noise args retval]
  trace/ITrace
  (get-args [_] args)
  (get-retval [_] retval)
  (get-gen-fn [_] gen-fn)
  (get-choices [_] (->ChoiceMap trie))
  (get-score [_] score))

(defn trace
  "Returns a new bare trace.

  TODO pad args with defaults if available."
  [gen-fn args]
  (Trace. gen-fn {} 0.0 0.0 args nil))

(defn validate-empty! [m addr]
  (when (contains? m addr)
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
  "Combine trace update states. careful not to add "
  [v k {:keys [trace weight discard]}]
  {:trace   (add-call (:trace v) k trace)
   :weight  (+ (:weight v) weight)
   :discard (if (empty? discard)
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
        to-subtract (reduce-kv (fn [acc _ v] (+ acc v))
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
             ([^gen.dynamic.DynamicDSLFunction gf args]
              (apply (.-clojure-fn gf) args))
             ([k gen-fn args]
              (validate-empty! (:trace @state) k)
              (let [k-constraints (choicemap/get-submap constraints k)
                    new-state
                    (if-let [prev-subtrace (get (.-trie this) k)]
                      (do
                        (assert
                         (= gen-fn (trace/get-gen-fn prev-subtrace))
                         (str "Generative function changed at address " k "."))
                        (trace/update prev-subtrace k-constraints))
                      (gf/generate gen-fn args k-constraints))]
                (swap! state combine k new-state)
                (trace/get-retval
                 (:subtrace new-state)))))]
        (let [inner-f                        (.-clojure-fn ^gen.dynamic.DynamicDSLFunction gen-fn)
              retval                         (apply inner-f args)
              {:keys [trace weight discard]} @state
              [to-subtract unvisited]        (extract-unvisited this trace)]
          (assert-all-visited! trace constraints)
          {:trace   (with-retval trace retval)
           :change  diff/unknown-change
           :weight  (- weight to-subtract)
           :discard (reduce conj discard unvisited)})))))

;; so we are going to remove the score of the unvisited stuff as we go up. Does
;; that work?

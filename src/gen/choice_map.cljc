(ns gen.choice-map
  "Protocol defining the choice map interface."
  (:refer-clojure :exclude [to-array merge]))

;; TODO value choicemap, get-value for no address?

(defprotocol IChoiceMap
  (-has-value? [m])
  (-get-value [m])
  (has-submap? [m k])
  (get-submap [m k] "NOTE that this returns a submap, not a value.")
  (get-values-shallow [m] "Return just values")
  (get-submaps-shallow [m] "Returns just submaps"))

(defn choice-map? [x]
  (satisfies? IChoiceMap x))

(defn has-value?
  ([m] (-has-value? m))
  ([m k] (-has-value? (get-submap m k))))

(defn get-value
  ([m] (-get-value m))
  ([m k] (-get-value (get-submap m k))))

(defprotocol IArray
  (to-array [m])
  (-from-array [m a i]))

(defn from-array [m xs]
  (let [[n ret] (-from-array m xs 0)]
    (if (= n (count xs))
      ret
      (throw (ex-info "Dimension mismatch: " {:xs xs :n n})))))

;; ## Empty

(defrecord EmptyChoiceMap []
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ _] false)
  (get-submap [this _] this)
  (get-values-shallow [_] {})
  (get-submaps-shallow [_] {})

  IArray
  (to-array [_] [])
  (-from-array [this _ _] [0 this]))

(def EMPTY
  "Empty choicemap singleton."
  (->EmptyChoiceMap))

;; ## Value

(defrecord Choice [v]
  IChoiceMap
  (-has-value? [_] true)
  (-get-value [_] v)
  (has-submap? [_ _] false)
  (get-submap [_ _] EMPTY)
  (get-values-shallow [_] {})
  (get-submaps-shallow [_] {})

  IArray
  (to-array [_] [v])
  (-from-array [_ xs idx]
    [1 (nth xs idx)]))

;; ## Vector

(defrecord VectorChoiceMap [v]
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ i] (contains? v i))
  (get-submap [_ i] (nth v i EMPTY))
  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc i x]
        (if (has-value? x)
          (assoc! acc i (get-value x))
          acc))
      (transient {})
      v)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv assoc! (transient {}) v)))

  IArray
  (to-array [_]
    (into [] (mapcat to-array) v))

  (-from-array [_ xs start-idx]
    (let [n (count v)]
      (loop [i      0
             offset start-idx
             acc    (transient [])]
        (if (< i n)
          (let [x       (nth v i nil)
                [n ret] (-from-array x xs offset)]
            (recur (inc i)
                   (+ n offset)
                   (conj! acc ret)))
          [(- offset start-idx)
           (VectorChoiceMap.
            (persistent! acc))])))))

(declare ->map)

(deftype DynamicChoiceMap [m]
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ i] (contains? m i))
  (get-submap [_ i] (get m i EMPTY))
  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (if (has-value? v)
          (assoc! acc k (get-value v))
          acc))
      (transient {})
      m)))

  (get-submaps-shallow [_] m)

  IArray
  (to-array [_]
    (let [pairs (sort-by key m)
          xform (mapcat
                 (fn [[_ v]]
                   (to-array v)))]
      (into [] xform pairs)))

  (-from-array [_ xs start-idx]
    (let [pairs (into [] (sort-by key m))
          n     (count pairs)]
      (loop [i      0
             offset start-idx
             acc    (transient {})]
        (if (< i n)
          (let [[k v]   (nth pairs i nil)
                [n ret] (-from-array v xs offset)]
            (recur (inc i)
                   (+ n offset)
                   (assoc! acc k ret)))
          [(- offset start-idx)
           (DynamicChoiceMap.
            (persistent! acc))])))))

;; ## API

(defn vector-> [v]
  (->VectorChoiceMap v))

(defn choicemap
  ([] (->DynamicChoiceMap {}))
  ([m]
   (let [f (fn [acc k v]
             (assoc! acc k (if (map? v)
                             (choicemap v)
                             (->Choice v))))]
     (->DynamicChoiceMap
      (persistent!
       (reduce-kv f (transient {}) m))))))

(defn cm:assoc [^DynamicChoiceMap m k v]
  (->DynamicChoiceMap
   (assoc (.-m m) k v)))

(defn cm:assoc-in
  "TODO note that this will be careful re:assignment."
  [^DynamicChoiceMap cm [k & ks] v]
  (if ks
    (let [sub-m (get (.-m cm) k (choicemap))]
      (if (has-value? sub-m)
        (throw (ex-info "Already a value at `k`, tried to assign nested" {}))
        (cm:assoc cm k (cm:assoc-in sub-m ks v))))
    (cm:assoc cm k v)))

(defn ->map
  "Returns a map with all entries in the choicemap.

  NOTE that this will lose the distinctions between a submap and a value of type
  `map`."
  [cm]
  (if (choice-map? cm)
    (if (has-value? cm)
      (get-value cm)
      (update-vals (get-submaps-shallow cm)
                   ->map))
    cm))

(defn cm:empty?
  "TODO replace"
  [v]
  (if (choice-map? v)
    (instance? EmptyChoiceMap v)
    (empty? v)))

(defn merge
  ([] EMPTY)
  ([m] m)
  ([l r]
   (cond (cm:empty? l) r
         (cm:empty? r) l

         (or (has-value? l) (has-value? r))
         (throw (ex-info "Can't merge values." {}))

         :else
         (->DynamicChoiceMap
          (merge-with merge
                      (get-submaps-shallow l)
                      (get-submaps-shallow r)))))
  ([l r & more]
   (reduce merge (merge l r) more)))

(defn ^:no-doc parse-choice-map
  "Implementation of a reader literal that turns literal map forms into calls
  to [[choice-map]].

  Installed by default under `#gen/choice-map`."
  [form]
  `(choicemap ~form))

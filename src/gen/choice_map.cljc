(ns gen.choice-map
  "Protocol defining the choice map interface."
  (:refer-clojure :exclude [to-array]))

(defprotocol IChoiceMap
  (has-value? [m k])
  (get-value [m k] "NOTE that this returns a value, not a submap.")
  (has-submap? [m k])
  (get-submap [m k] "NOTE that this returns a submap, not a value.")
  (get-values-shallow [m] "Return just values")
  (get-submaps-shallow [m] "Returns just submaps"))

(defn choice-map? [x]
  (satisfies? IChoiceMap x))

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
  (has-value? [_ _] false)
  (get-value [_ _] nil)
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

;; ## Vector

(defrecord VectorChoiceMap [v]
  IChoiceMap
  (has-value? [_ i]
    (and (contains? v i)
         (not (choice-map? (nth v i nil)))))

  (get-value [_ i]
    (let [x (nth v i nil)]
      (when-not (choice-map? x)
        x)))

  (has-submap? [_ i]
    (and (contains? v i)
         (choice-map? (nth v i nil))))

  (get-submap [_ i]
    (let [x (nth v i nil)]
      (when (choice-map? x)
        x)))

  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc i x]
        (if-not (choice-map? x)
          (assoc! acc i x)
          acc))
      (transient {})
      v)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc i v]
        (if (choice-map? v)
          (assoc! acc i v)
          acc))
      (transient {})
      v)))

  IArray
  (to-array [_]
    (into []
          (mapcat (fn [x]
                    (if (choice-map? x)
                      (to-array x)
                      [x])))
          v))

  (-from-array [_ xs start-idx]
    (let [n (count v)]
      (loop [i      0
             offset start-idx
             acc    []]
        (if (< i n)
          (let [x (nth v i nil)]
            (if (choice-map? x)
              (let [[n ret] (-from-array x xs offset)]
                (recur (inc i)
                       (+ n offset)
                       (conj acc ret)))
              (recur (inc i)
                     (inc offset)
                     (conj acc (nth xs offset nil)))))
          [(- offset start-idx)
           (VectorChoiceMap. acc)])))))

(declare ->map)

(deftype DynamicChoiceMap [leaves nodes]
  IChoiceMap
  (has-value? [_ i] (contains? leaves i))
  (get-value [_ i] (get leaves i nil))
  (has-submap? [_ i] (contains? nodes i))
  (get-submap [_ i] (get nodes i nil))
  (get-values-shallow [_] leaves)
  (get-submaps-shallow [_] nodes)

  IArray
  (to-array [_]
    (let [m     (into leaves nodes)
          pairs (sort-by key m)
          xform (mapcat
                 (fn [[_ v]]
                   (if (choice-map? v)
                     (to-array v)
                     [v])))]
      (into [] xform pairs)))

  (-from-array [_ xs start-idx]
    (let [m     (into leaves nodes)
          pairs (into [] (sort-by key m))
          n     (count pairs)]
      (loop [i      0
             offset start-idx
             l-acc  (transient {})
             n-acc  (transient {})]
        (if (< i n)
          (let [[k v] (nth pairs i nil)]
            (if (choice-map? v)
              (let [[n ret] (-from-array v xs offset)]
                (recur (inc i)
                       (+ n offset)
                       l-acc
                       (assoc! n-acc k ret)))
              (recur (inc i)
                     (inc offset)
                     (assoc! l-acc k (nth xs offset nil))
                     n-acc)))
          [(- offset start-idx)
           (DynamicChoiceMap.
            (persistent! l-acc)
            (persistent! n-acc))])))))

(extend-protocol IArray
  #?(:clj Object :cljs default)
  (to-array [x] [x])
  (-from-array [_ xs idx]
    [1 (nth xs idx)])


  ;; TODO fix by making this call recursively.
  ;; #?(:clj IPersistentVector :cljs PersistentVector)
  ;; (to-array [v] v)
  ;; (-from-array [v xs start-idx]
  ;;   (let [n (count v)]
  ;;     [n (subvec xs start-idx (+ start-idx n))]))
  )

;; ## API

(defn vector-> [v]
  (->VectorChoiceMap v))

(defn choicemap
  ([] (->DynamicChoiceMap {} {}))
  ([m]
   (let [f (fn [[l n] k v]
             (if (choice-map? v)
               [l (assoc! n k v)]
               [(assoc! l k v) n]))
         [l n] (reduce-kv f
                          [(transient {})
                           (transient {})]
                          m)]
     (->DynamicChoiceMap
      (persistent! l)
      (persistent! n)))))

(defn ^:no-doc assoc-leaf
  [^DynamicChoiceMap m k v]
  (->DynamicChoiceMap (assoc (.-leaves m) k v)
                      (dissoc (.-nodes m) k)))

(defn ^:no-doc assoc-node
  "TODO implement empty? and don't assoc an empty node."
  [^DynamicChoiceMap m k v]
  (->DynamicChoiceMap (dissoc (.-leaves m) k)
                      (assoc (.-nodes m) k v)))

(defn cm:assoc [m k v]
  (let [assoc-f (if (choice-map? v)
                  assoc-node
                  assoc-leaf)]
    (assoc-f m k v)))

(defn cm:assoc-in
  "TODO note that this will be careful re:assignment."
  [^DynamicChoiceMap m [k & ks] v]
  (if ks
    (if (has-value? m k)
      (throw (ex-info "Already a value at `k`, tried to assign nested" {}))
      (let [sub-m (if (has-submap? m k)
                    (get-submap m k)
                    (choicemap))]
        (cm:assoc m k (cm:assoc-in sub-m ks v))))
    (cm:assoc m k v)))

(defn ->map
  "Returns a map with all entries in the choicemap.

  NOTE that this will lose the distinctions between a submap and a value of type
  `map`."
  [cm]
  (if (choice-map? cm)
    (let [vals    (get-values-shallow cm)
          submaps (get-submaps-shallow cm)]
      (into vals (update-vals submaps ->map)))
    cm))

(defn merge
  "TODO simplify...

  TODO implement conj better so that we detect submap vs value?"
  ([] EMPTY)
  ([m] m)
  ([l r]
   (cond (not l) r
         (not r) l
         :else
         (let [acc (->DynamicChoiceMap (get-values-shallow l) {})
               acc (reduce-kv (fn [acc k l-node]
                                (let [r-node (get-submap r k)]
                                  (assoc-node acc k (merge l-node r-node))))
                              acc
                              (get-submaps-shallow l))
               acc (reduce-kv (fn [acc k r-leaf]
                                (cond (has-value? acc k)
                                      (throw (ex-info "clash at k"
                                                      {:k k
                                                       :l (get-value acc k)
                                                       :r r-leaf}))

                                      (has-submap? acc k)
                                      (throw (ex-info "clash at k"
                                                      {:k k
                                                       :l (get-submap acc k)
                                                       :r r-leaf}))
                                      :else (assoc-leaf acc k r-leaf)))
                              acc
                              (get-values-shallow r))]
           ;; at this point we have all values, all left-submaps, and we
           ;; want to collect the remaining right submaps. and make sure
           ;; there is no key clash.
           ;;
           ;; we should diff the keysets, but just copying for now.
           (reduce-kv (fn [acc k r-node]
                        (cond (has-value? acc k)
                              (throw (ex-info "clash at k"
                                              {:k k
                                               :acc (get-value acc k)
                                               :r r-node}))
                              (has-submap? acc k)
                              acc

                              :else (assoc-node acc k r-node)))
                      acc
                      (get-submaps-shallow r)))))
  ([l r & more]
   (reduce merge (merge l r) more)))

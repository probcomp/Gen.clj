(ns gen.choice-map
  "Protocol defining the choice map interface."
  (:refer-clojure :exclude [to-array])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap
                            IMapIterable MapEntry))))

(defprotocol IChoiceMap
  (has-value? [m k])
  (get-value [m k] "NOTE that this returns a value, not a submap.")
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

(deftype MapChoiceMap [leaves nodes]
  IChoiceMap
  (has-value? [_ i] (contains? leaves i))
  (get-value [_ i] (get leaves i nil))
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
           (MapChoiceMap.
            (persistent! l-acc)
            (persistent! n-acc))])))))

;; ## API

(defn vector-> [v]
  (->VectorChoiceMap v))

(defn map-> [m]
  (let [f (fn [[l n] k v]
            (if (choice-map? v)
              [l (assoc! n k v)]
              [(assoc! l k v) n]))
        [l n] (reduce-kv f
                         [(transient {})
                          (transient {})]
                         m)]
    (->MapChoiceMap (persistent! l)
                    (persistent! n))))

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

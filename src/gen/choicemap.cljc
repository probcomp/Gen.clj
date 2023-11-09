(ns gen.choicemap
  (:refer-clojure :exclude [assoc-in merge])
  (:require [clojure.pprint :as pprint]
            [gen.array :as arr])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap))))

;; ## Choice Maps
;;
;;
;; You should implement TODO describe what's implemented in each of these types.

;; ;; - ISeqable to get `empty?` working

(defprotocol IChoiceMap
  (-has-value? [m])
  (-get-value [m])
  (has-submap? [m k])
  (get-submap [m k] "NOTE that this returns a submap, not a value.")
  (get-values-shallow [m] "Return just values")
  (get-submaps-shallow [m] "Returns just submaps"))

(defn choicemap? [x]
  (satisfies? IChoiceMap x))

(defn has-value?
  ([m] (-has-value? m))
  ([m k] (-has-value? (get-submap m k))))

(defn get-value
  ([m] (-get-value m))
  ([m k] (-get-value (get-submap m k))))

;; ## Value

(declare EMPTY)

(deftype Choice [v]
  IChoiceMap
  (-has-value? [_] true)
  (-get-value [_] v)
  (has-submap? [_ _] false)
  (get-submap [_ _] EMPTY)
  (get-values-shallow [_] {})
  (get-submaps-shallow [_] {})

  arr/IArray
  (to-array [_] [v])
  (-from-array [_ xs idx]
    [1 (Choice. (nth xs idx))])

  #?@(:clj
      [Object
       (toString [this] (pr-str this))
       (equals [_ o]
               (and (instance? Choice o)
                    (= v (.-v ^Choice o))))]

      :cljs
      [Object
       (toString [this] (pr-str this))
       (equiv [this other] (-equiv this other))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-write writer "#gen/choice ")
                   (-pr-writer v writer opts))

       IEquiv
       (-equiv [_ o]
               (and (instance? Choice o)
                    (= v (.-v ^Choice o))))]))

#?(:clj
   (defmethod print-method Choice
     [^Choice choice ^java.io.Writer w]
     (.write w "#gen/choice ")
     (.write w (pr-str (.-v choice)))))

(defmethod pprint/simple-dispatch Choice [^Choice c]
  #?(:clj (.write ^java.io.Writer *out* "#gen/choice ")
     :cljs (-write *out* "#gen/choice "))
  (pprint/simple-dispatch (.-v c)))

;; ## Empty

(declare kv->choicemap)

(deftype EmptyChoiceMap [m]
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ _] false)
  (get-submap [this _] this)
  (get-values-shallow [_] {})
  (get-submaps-shallow [_] {})

  arr/IArray
  (to-array [_] [])
  (-from-array [this _ _] [0 this])

  #?@(:clj
      [Object
       (equals [_ o] (instance? EmptyChoiceMap o))
       (toString [this] (pr-str this))

       IFn
       (invoke [_ _] nil)
       (invoke [_ _ not-found] not-found)

       IObj
       (meta [_] m)
       (withMeta [_ meta-m] (EmptyChoiceMap. meta-m))

       IPersistentMap
       (assocEx [_ k v] (kv->choicemap k v))
       (assoc [_ k v] (kv->choicemap k v))
       (without [this _] this)

       Associative
       (containsKey [_ _] false)
       (entryAt [_ _] nil)
       (cons [this o]
             (if (map? o)
               (reduce-kv assoc this o)
               (let [[k v] o]
                 (kv->choicemap k v))))
       (count [_] 0)
       (seq [_] nil)
       (empty [_] (EmptyChoiceMap. nil))
       (valAt [_ _] nil)
       (valAt [_ _ not-found] not-found)
       (equiv [_ o] (instance? EmptyChoiceMap o))]

      :cljs
      [Object
       (toString [this] (pr-str this))
       (equiv [this other] (-equiv this other))

       IPrintWithWriter
       (-pr-writer [_ writer _]
                   (-write writer "#gen/choicemap {}"))

       IFn
       (-invoke [_ _] nil)
       (-invoke [_ _ not-found] not-found)

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ meta-m] (EmptyChoiceMap. meta-m))

       IEmptyableCollection
       (-empty [_] (EmptyChoiceMap. nil))

       IEquiv
       (-equiv [_ o] (instance? EmptyChoiceMap o))

       ISeqable
       (-seq [_] nil)

       ICounted
       (-count [_] 0)

       ILookup
       (-lookup [_ _] nil)
       (-lookup [_ _ not-found] not-found)

       IAssociative
       (-assoc [_ k v] (kv->choicemap k v))
       (-contains-key? [_ _] false)

       IMap
       (-dissoc [this _] this)]))

#?(:clj
   (defmethod print-method EmptyChoiceMap
     [_ ^java.io.Writer w]
     (.write w "#gen/choicemap {}")))

(defmethod pprint/simple-dispatch EmptyChoiceMap [_]
  #?(:clj  (.write ^java.io.Writer *out* "#gen/choicemap {}")
     :cljs (-write *out* "#gen/choicemap {}")))

(def EMPTY
  "Empty choicemap singleton."
  (->EmptyChoiceMap nil))

;; ## Map-shaped Choice Map

;; The map here has the invariant that values are always other choicemaps.

(declare equiv choicemap)

(deftype DynamicChoiceMap [m]
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ k] (contains? m k))
  (get-submap [_ k] (get m k EMPTY))
  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (if (-has-value? v)
          (assoc! acc k (-get-value v))
          acc))
      (transient {})
      m)))

  (get-submaps-shallow [_] m)

  arr/IArray
  (to-array [_]
    (let [pairs (sort-by key m)
          xform (mapcat
                 (fn [[_ v]] (arr/to-array v)))]
      (into [] xform pairs)))

  (-from-array [_ xs start-idx]
    (let [pairs (into [] (sort-by key m))
          n     (count pairs)]
      (loop [i      0
             offset start-idx
             acc    (transient {})]
        (if (< i n)
          (let [[k v]   (nth pairs i nil)
                [n ret] (arr/-from-array v xs offset)]
            (recur (inc i)
                   (+ n offset)
                   (assoc! acc k ret)))
          [(- offset start-idx)
           (DynamicChoiceMap.
            (persistent! acc))]))))

  #?@(:clj
      [Object
       (equals [this that] (equiv this that))
       (toString [this] (pr-str this))

       IFn
       (invoke [_ k] (m k))
       (invoke [_ k not-found] (m k not-found))

       IObj
       (meta [_] (meta m))
       (withMeta [_ meta-m]
                 (DynamicChoiceMap.
                  (with-meta m meta-m)))

       IPersistentMap
       (assocEx [_ _ _] (throw (Exception.)))
       (assoc [_ k v]
              (DynamicChoiceMap.
               (assoc m k (choicemap v))))
       (without [m k] (DynamicChoiceMap. (dissoc m k)))

       Associative
       (containsKey [_ k] (contains? m k))
       (entryAt [_ k] (.entryAt ^Associative m k))
       (cons [this o]
             (if (map? o)
               (reduce-kv assoc this o)
               (if-let [[k v] (seq o)]
                 (assoc this k v)
                 this)))

       (count [_] (count m))
       (seq [_] (seq m))
       (empty [_] EMPTY)
       (valAt [_ k] (.valAt ^Associative m k))
       (valAt [_ k not-found] (.valAt ^Associative m k not-found))
       (equiv [this that] (equiv this that))]

      :cljs
      [Object
       (toString [_] (pr-str m))
       (equiv [this that] (equiv this that))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-write writer "#gen/choicemap ")
                   (-pr-writer m writer opts))

       IFn
       (-invoke [_ k] (-invoke m k))
       (-invoke [_ k not-found] (-invoke m k not-found))

       IMeta
       (-meta [_] (-meta m))

       IWithMeta
       (-with-meta [_ meta-m]
                   (DynamicChoiceMap.
                    (-with-meta m meta-m)))

       IEmptyableCollection
       (-empty [_] EMPTY)

       IEquiv
       (-equiv [this that] (equiv this that))

       ISeqable
       (-seq [_] (-seq m))

       ICounted
       (-count [_] (-count m))

       ILookup
       (-lookup [_ k] (-lookup m k))
       (-lookup [_ k not-found] (-lookup m k not-found))

       IAssociative
       (-assoc [_ k v]
               (DynamicChoiceMap.
                (assoc m k (choicemap v))))
       (-contains-key? [_ k] (-contains-key? m k))

       IMap
       (-dissoc [_ k]
                (DynamicChoiceMap.
                 (dissoc m k)))]))

#?(:clj
   (defmethod print-method DynamicChoiceMap
     [^DynamicChoiceMap m ^java.io.Writer w]
     (.write w "#gen/choicemap ")
     (print-method (.-m m) w)))

(defmethod pprint/simple-dispatch DynamicChoiceMap
  [^DynamicChoiceMap m]
  #?(:clj (.write ^java.io.Writer *out* "#gen/choicemap ")
     :cljs (-write *out* "#gen/choicemap "))
  (pprint/simple-dispatch (.-m m)))

;; ## Vector

(deftype VectorChoiceMap [v]
  IChoiceMap
  (-has-value? [_] false)
  (-get-value [_] nil)
  (has-submap? [_ i] (contains? v i))
  (get-submap [_ i] (nth v i EMPTY))
  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc i x]
        (if (-has-value? x)
          (assoc! acc i (-get-value x))
          acc))
      (transient {})
      v)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv assoc! (transient {}) v)))

  arr/IArray
  (to-array [_]
    (into [] (mapcat arr/to-array) v))

  (-from-array [_ xs start-idx]
    (let [n (count v)]
      (loop [i      0
             offset start-idx
             acc    (transient [])]
        (if (< i n)
          (let [x       (nth v i nil)
                [n ret] (arr/-from-array x xs offset)]
            (recur (inc i)
                   (+ n offset)
                   (conj! acc ret)))
          [(- offset start-idx)
           (VectorChoiceMap.
            (persistent! acc))]))))

  #?@(:clj
      [Object
       (equals [this that] (equiv this that))
       (toString [this] (pr-str this))

       IFn
       (invoke [_ k] (v k))
       (invoke [_ k not-found] (v k not-found))

       IObj
       (meta [_] (meta v))
       (withMeta [_ meta-m]
                 (VectorChoiceMap.
                  (with-meta v meta-m)))

       IPersistentMap
       (assocEx [_ _ _] (throw (Exception.)))
       (assoc [this k val]
              (if (<= 0 k (count v))
                (VectorChoiceMap.
                 (assoc v k (choicemap val)))
                (-> (get-submaps-shallow this)
                    (assoc k (choicemap val))
                    (DynamicChoiceMap.))))

       (without [this k]
                (if (contains? v k)
                  (-> (get-submaps-shallow this)
                      (dissoc k)
                      (DynamicChoiceMap.))
                  this))

       Associative
       (containsKey [_ k] (contains? v k))
       (entryAt [_ k] (.entryAt ^Associative v k))
       (cons [_ val]
             (VectorChoiceMap.
              (conj v (choicemap val))))

       (count [_] (count v))
       (seq [_] (seq v))
       (empty [_] EMPTY)
       (valAt [_ k] (.valAt ^Associative v k))
       (valAt [_ k not-found] (.valAt ^Associative v k not-found))
       (equiv [this that] (equiv this that))]

      :cljs
      [Object
       (toString [_] (pr-str v))
       (equiv [this that] (equiv this that))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-write writer "#gen/choicemap ")
                   (-pr-writer v writer opts))

       IFn
       (-invoke [_ k] (-invoke v k))
       (-invoke [_ k not-found] (-invoke v k not-found))

       IMeta
       (-meta [_] (-meta v))

       IWithMeta
       (-with-meta [_ meta-m]
                   (VectorChoiceMap.
                    (-with-meta v meta-m)))

       IEmptyableCollection
       (-empty [_] EMPTY)

       IEquiv
       (-equiv [this that] (equiv this that))

       ISeqable
       (-seq [_] (-seq v))

       ICounted
       (-count [_] (-count v))

       ILookup
       (-lookup [_ k] (-lookup v k))
       (-lookup [_ k not-found] (-lookup v k not-found))

       IAssociative
       (-assoc [this k v]
               (if (<= 0 k (count v))
                 (VectorChoiceMap.
                  (assoc v k (choicemap val)))
                 (-> (get-submaps-shallow this)
                     (assoc k (choicemap val))
                     (DynamicChoiceMap.))))

       (-contains-key? [_ k] (-contains-key? v k))

       IMap
       (-dissoc [this k]
                (if (contains? v k)
                  (-> (get-submaps-shallow this)
                      (dissoc k)
                      (DynamicChoiceMap.))
                  this))]))

#?(:clj
   (defmethod print-method VectorChoiceMap
     [^VectorChoiceMap m ^java.io.Writer w]
     (.write w "#gen/choicemap ")
     (print-method (.-v m) w)))

#?(:clj
   (defmethod pprint/simple-dispatch VectorChoiceMap
     [^VectorChoiceMap m]
     (.write ^java.io.Writer *out* "#gen/choicemap ")
     (pprint/simple-dispatch (.-v m))))

(declare choicemap)

(defn ^:no-doc parse-choice
  "Implementation of a reader literal that turns literal forms into calls
  to [[->Choice]].

  Installed by default under `#gen/choice`."
  [form]
  `(->Choice ~form))

(defn ^:no-doc parse-choicemap
  "Implementation of a reader literal that turns literal map forms into calls
  to [[choicemap]].

  Installed by default under `#gen/choicemap`."
  [form]
  `(choicemap ~form))

;; ## API

(defn ^:no-doc equiv
  "Assumes that `l` is a [[ChoiceMap]]."
  [l r]
  (and (choicemap? r)
       (= (get-submaps-shallow l)
          (get-submaps-shallow r))))

(defn ^:no-doc kv->choicemap
  "Generates a choicemap from the supplied kv pair."
  [k v]
  (->DynamicChoiceMap {k (choicemap v)}))

(defn ^:no-doc map->choicemap
  "make one from a map"
  [m]
  (if (seq m)
    (let [f (fn [acc k v]
              (assoc! acc k (choicemap v)))]
      (->DynamicChoiceMap
       (persistent!
        (reduce-kv f (transient {}) m))))
    EMPTY))

(defn ^:no-doc vector->choicemap [v]
  (if (seq v)
    (->VectorChoiceMap
     (into [] (map choicemap) v))
    EMPTY))

(defn choicemap
  "Returns a choicemap, vector map etc TODO describe!"
  ([] EMPTY)
  ([x]
   (cond (choicemap? x) x
         (vector? x)     (vector->choicemap x)
         (map? x)        (map->choicemap x)
         :else           (->Choice x))))

(defn ->map
  "Returns a map with all entries in the choicemap.

  NOTE that this will lose the distinctions between a submap and a value of type
  `map`."
  [cm]
  (if (choicemap? cm)
    (if (has-value? cm)
      (get-value cm)
      (update-vals (get-submaps-shallow cm)
                   ->map))
    cm))

;; ## Methods

(defn assoc-in
  "TODO note that this will be careful re:assignment! assoc-in will work but this will error."
  [cm [k & ks] v]
  (if ks
    (let [sub-m (get cm k EMPTY)]
      (if (has-value? sub-m)
        (throw
         (ex-info "Already a value at `k`, tried to assign nested"
                  {:k k :v sub-m}))
        (assoc cm k (assoc-in sub-m ks v))))
    (assoc cm k v)))

(defn merge
  ([] EMPTY)
  ([m] m)
  ([l r]
   (cond (empty? l) r
         (empty? r) l

         (or (has-value? l) (has-value? r))
         (throw (ex-info "Can't merge values." {}))

         :else
         (->DynamicChoiceMap
          (merge-with merge
                      (get-submaps-shallow l)
                      (get-submaps-shallow r)))))
  ([l r & more]
   (reduce merge (merge l r) more)))

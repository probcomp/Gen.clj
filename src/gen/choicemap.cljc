(ns gen.choicemap
  "Defines the [[IChoiceMap]] abstraction, its API and a number of out-of-the-box
  implementations of leaves and nodes of choicemaps."
  (:refer-clojure :exclude [assoc-in merge empty?])
  (:require [clojure.core :as core]
            [clojure.pprint :as pprint]
            [gen.array :as arr])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap))))

;; ## Choice Map
;;
;; [[IChoiceMap]] is a tree-like abstraction used by Gen to present the random
;; choices stored inside an instance of [[gen.trace/ITrace]].
;;
;; The two main types of choice maps are
;;
;; - [[Choice]] instances, referred to in other Gen implementations
;;   as "ValueChoiceMap"
;; - Hierarchical choice maps that maintain a mapping from key to [[IChoiceMap]]
;;   instance.
;;
;; The first two functions in the protocol concern [[Choice]] instances, or
;; leaves of the [[IChoiceMap]] tree. The other four are used by nodes, i.e.,
;; hierarchical choice maps.

(defprotocol IChoiceMap
  (-has-value? [m]
    "Returns true if `m` is a leaf, false otherwise.")

  (-get-value [m]
    "Returns the stored value if `m` is a leaf, nil otherwise. ")

  (has-submap? [m k]
    "Returns true if `m` is storing a mapping from `k` to another [[IChoiceMap]]
    instance, false otherwise.")

  (get-submap [m k]
    "Returns true if `m` is storing a mapping from `k` to another [[IChoiceMap]]
    instance, false otherwise.

    NOTE: [[get-submap]] always returns an [[IChoiceMap]], even
    if [[has-submap?]] returns false for `k`.")

  (get-values-shallow [m]
    "Returns a map of address => leaf [[IChoiceMap]] instances (i.e. instances
    that return true for [[has-value?]]).")

  (get-submaps-shallow [m]
    "Returns a map of address => all stored [[IChoiceMap]] instances, both
    leaves and nodes."))

(defn choicemap?
  "Returns true if `x` implements [[IChoiceMap]], false otherwise."
  [x]
  (satisfies? IChoiceMap x))

(defn has-value?
  "If no `k` is provided, returns true if `m` is a non-hierarchical [[IChoiceMap]]
  implementer, and contains a concrete value, false otherwise.

  If a `k` is provided, returns true if `m` is a hierarchical [[IChoiceMap]]
  with a non-hierarchical, value-containing submap at address `k`."
  ([m] (-has-value? m))
  ([m k] (-has-value? (get-submap m k))))

(defn get-value
  "If no `k` is provided, if `m` returns true for `has-value?`, returns the value
  stored in `m`, nil otherwise.

  If a `k` is provided, returns the value stored at the [[IChoiceMap]] instance
  at address `k` in `m`, or `nil` if that address is empty or contains a
  hierarchical [[IChoiceMap]]."
  ([m] (-get-value m))
  ([m k] (-get-value (get-submap m k))))

;; ## Choice
;;
;; This first type is a non-hierarchical [[IChoiceMap]] instance, essentially a
;; wrapper for leaf nodes in a structured choice map. Many [[IChoiceMap]]
;; implementations will make use of this same leaf type.
;;
;; Rather than extending this implementation to `object`, we require an
;; explicit [[Choice]] wrapper for parity with Gen.jl's implementation, and to
;; make the [[IChoiceMap]] interface opt-in.

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

#?(:cljs
   (extend-type default
     IPrintWithWriter
     (-pr-writer [n writer _]
       (-write writer n))))

#?(:clj
   (defmethod print-method Choice
     [^Choice choice ^java.io.Writer w]
     (.write w "#gen/choice ")
     (print-method (.-v choice) w)))

(defmethod pprint/simple-dispatch Choice [^Choice c]
  #?(:clj (.write ^java.io.Writer *out* "#gen/choice ")
     :cljs (-write *out* "#gen/choice "))
  (pprint/simple-dispatch (.-v c)))

;; ## Map-shaped Choice Map

;; The [[DynamicChoiceMap]] implementation is for hierarchical,
;; non-value-containing [[IChoiceMap]]s. This type maintains the invariant that
;; the values of `m` are always other [[IChoiceMap]]s.

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
       (equiv [this that] (equiv this that))

       Iterable
       (iterator [_] (.iterator ^Iterable m))]

      :cljs
      [Object
       (toString [_] (pr-str m))
       (equiv [this that] (equiv this that))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-write writer "#gen/choicemap ")
                   (-pr-writer m writer opts))

       IFn
       (-invoke [_ k] (-lookup m k))
       (-invoke [_ k not-found] (-lookup m k not-found))

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

       ICollection
       (-conj [this entry]
              (if (map? entry)
                (reduce-kv assoc this entry)
                (if-let [[k v] entry]
                  (assoc this k v)
                  this)))

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

;; ## Vector-shaped Choice Maps
;;
;; [[VectorChoiceMap]] tries to be similar to [[DynamicChoiceMap]], but more
;; efficient for sequential, numerical addresses (like the indices of a vector).
;;
;; This type will attempt to stay a vector, but will convert to
;; a [[DynamicChoiceMap]] if you do something like `assoc` an address outside of
;; its range.

(declare v:assoc)

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
       (invoke [_ k not-found]
               (if (contains? v k)
                 (v k)
                 not-found))

       IObj
       (meta [_] (meta v))
       (withMeta [_ meta-m]
                 (VectorChoiceMap.
                  (with-meta v meta-m)))

       IPersistentMap
       (assocEx [_ _ _] (throw (Exception.)))
       (assoc [this k val] (v:assoc this k val))

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
       (equiv [this that] (equiv this that))

       Iterable
       (iterator [_] (.iterator ^Iterable v))]

      :cljs
      [Object
       (toString [_] (pr-str v))
       (equiv [this that] (equiv this that))

       IPrintWithWriter
       (-pr-writer [_ writer opts]
                   (-write writer "#gen/choicemap ")
                   (-pr-writer v writer opts))

       IFn
       (-invoke [_ k] (-lookup v k))
       (-invoke [_ k not-found] (-lookup v k not-found))

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
       (-assoc [this k val] (v:assoc this k val))
       (-contains-key? [_ k] (-contains-key? v k))

       ICollection
       (-conj [_ val]
              (VectorChoiceMap.
               (conj v (choicemap val))))

       IMap
       (-dissoc [this k]
                (if (contains? v k)
                  (-> (get-submaps-shallow this)
                      (dissoc k)
                      (DynamicChoiceMap.))
                  this))]))

(defn- v:assoc [^VectorChoiceMap this k val]
  (let [v (.-v this)]
    (if (and (number? k)
             (<= 0 k (count v)))
      (VectorChoiceMap.
       (assoc v k (choicemap val)))
      (-> (get-submaps-shallow this)
          (assoc k (choicemap val))
          (DynamicChoiceMap.)))))

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

;; ## Reader Literals
;;
;; These next methods aren't exposed in the public API, but support reader
;; literals like `#gen/choice 10` or `#gen/choicemap [1 2 3]`.
;;
;; The former exists to make it possible to create explicit map-or-vector-shaped
;; leaves, instead of having them auto-converted into choicemap wrapper.

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

;; ### Empty

;; Other implementations should return this from [[get-submap]] (instead of
;; `nil`) in the case of queries for a missing address.

(def EMPTY
  "Empty choicemap singleton instance."
  (->DynamicChoiceMap {}))

;; ### Constructors

(defn ^:no-doc kv->choicemap
  "Generates a [[DynamicChoiceMap]] from the supplied (`k`, `v`) pair. Used
  internally in cases where we have a single entry."
  [k v]
  (->DynamicChoiceMap {k (choicemap v)}))

(defn ^:no-doc map->choicemap
  "Generates a [[DynamicChoiceMap]] instance from the supplied map `m` by
  recursively calling [[choicemap]] on all values."
  [m]
  (if (seq m)
    (let [f (fn [acc k v]
              (assoc! acc k (choicemap v)))]
      (->DynamicChoiceMap
       (persistent!
        (reduce-kv f (transient {}) m))))
    EMPTY))

(defn ^:no-doc vector->choicemap
  "Generates a [[VectorChoiceMap]] instance from the supplied vector `v` by
  recursively calling [[choicemap]] on all entries."
  [v]
  (if (seq v)
    (->VectorChoiceMap
     (into [] (map choicemap) v))
    EMPTY))

(defn choicemap
  "Returns an [[IChoiceMap]] instance generated from `x`. Sequence- or map-shaped
  inputs will recursively convert their entries via [[choicemap]]; all other
  types will generate a [[Choice]] instance.

  The 0-arity returns [[EMPTY]]."
  ([] EMPTY)
  ([x]
   (cond (choicemap? x) x

         (map? x)
         (map->choicemap x)

         (or (vector? x) (seq? x))
         (vector->choicemap x)

         :else (->Choice x))))

;; ### ChoiceMap interactions

(defn ^:no-doc equiv
  "Returns true if `r` is a choicemap with equivalent submaps to `l`, false
  otherwise.

  NOTE: [[equiv]] assumes that `l` is a [[ChoiceMap]]."
  [l r]
  (and (choicemap? r)
       (= (get-submaps-shallow l)
          (get-submaps-shallow r))))

(defn ->map
  "Given an [[IChoiceMap]] instance `cm`, returns a map generated by recursively calling [[get-submaps-shallow]] on all hierarchical entries and [[get-value]] on all value-containing entries.

  NOTE that this will lose the distinctions between a submap and a value of type
  `map`, if you happen to have map-shaped choices.

  Given any other type, acts as `identity`."
  [cm]
  (if (choicemap? cm)
    (if (has-value? cm)
      (get-value cm)
      (update-vals (get-submaps-shallow cm)
                   ->map))
    cm))

;; ## Methods

(defn assoc-in
  "Given an [[IChoiceMap]] instance `cm`, a sequence of addresses and a value `v`,
  attempts to generate a new [[IChoiceMap]] by recursively calling `assoc` on
  each submap.

  NOTE that unlike `clojure.core/assoc-in`, [[assoc-in]] will error if you try
  and pass a sequence of addresses that clashes with an existing value.
  Prefer [[assoc-in]] when you want this strict erroring behavior."
  [cm [k & ks] v]
  (if ks
    (let [sub-m (get cm k EMPTY)]
      (if (has-value? sub-m)
        (throw
         (ex-info
          "A value already exists at `k`, tried to assign a nested `path`."
          {:k k :v sub-m :path ks}))
        (assoc cm k (assoc-in sub-m ks v))))
    (assoc cm k v)))

(defn empty?
  "Returns true if `v` is a hierarchical [[IChoiceMap]] with no entries, false
  otherwise.

  For non-choicemap `v`s, returns `(clojure.core/empty? v)`."
  [v]
  (if (choicemap? v)
    (and (not (has-value? v))
         (core/empty? v))
    (core/empty? v)))

(defn merge
  "Given two or more [[IChoiceMap]] instances, returns a new [[DynamicChoiceMap]]
  instance generated by merging in the return values of [[get-submaps-shallow]]
  called on [[IChoiceMap]] instances `l` and `r`.

  Given a single argument `m`, acts as identity.

  Given no arguments, returns [[EMPTY]]."
  ([] EMPTY)
  ([m] m)
  ([l r]
   (cond (or (has-value? l) (has-value? r))
         (throw (ex-info "Can't merge values." {}))

         (core/empty? l) r
         (core/empty? r) l

         :else
         (->DynamicChoiceMap
          (merge-with merge
                      (get-submaps-shallow l)
                      (get-submaps-shallow r)))))
  ([l r & more]
   (reduce merge (merge l r) more)))

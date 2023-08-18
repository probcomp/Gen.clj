(ns gen.choice-map
  "Choice map interface and data structure."
  (:import (clojure.lang
            Associative IFn IObj IPersistentMap IMapIterable MapEntry)))

(declare choice-map choice-map?)

(declare ->map)

(defrecord Choice [choice])

(deftype ChoiceMap [m]
  Object
  (equals [_ o] (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))
  (toString [this] (pr-str this))

  IFn
  (invoke [this k] (.valAt this k))
  (invoke [this k not-found] (.valAt this k not-found))

  IObj
  (meta [_] (meta m))
  (withMeta [_ meta-m] (ChoiceMap. (with-meta m meta-m)))

  IPersistentMap
  (assocEx [_ _ _] (throw (Exception.)))
  (assoc   [_ k v]
    (ChoiceMap. (.assoc ^IPersistentMap m k v)))
  (without [_ k]
    (ChoiceMap. (.without ^IPersistentMap m k)))

  Associative
  (containsKey [_ k] (contains? m k))
  (entryAt [_ k]
    (when (contains? m k)
      (MapEntry/create k (get m k))))
  (cons [this o]
    (if (map? o)
      (reduce-kv assoc this o)
      (let [[k v] o]
        (ChoiceMap. (assoc m k v)))))
  (count [_] (count m))
  (seq [_]
    (when-let [kvs (seq m)]
      (map (fn [[k v]]
             (MapEntry/create k v))
           kvs)))
  (empty [_] (ChoiceMap. (empty m)))
  (valAt [_ k]
    (get m k))
  (valAt [_ k not-found]
    (get m k not-found))
  (equiv [_ o]
    (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

  IMapIterable
  (keyIterator [_] (.keyIterator ^IMapIterable m))
  (valIterator [_] (.valIterator ^IMapIterable m))

  Iterable
  (iterator [this] (.iterator ^Iterable (.seq this))))

(defmethod print-method ChoiceMap [^ChoiceMap cm ^java.io.Writer w]
  (.write w "#gen/choice-map ")
  (print-method (.-m cm) w))

;; ## Constructors


(defn make
  "Returns a [[ChoiceMap]] built out of the supplied key-value pairs."
  [& {:as m}]
  (->ChoiceMap (or m {})))

(def EMPTY (make))

(defn choice [x] x)

;; ## Predicates

(defn choice-map? [x]
  (instance? ChoiceMap x))

(defn choice?
  "TODO new, document"
  [x]
  (not (choice-map? x)))

;; ## API

(defn submaps [^ChoiceMap cm]
  (.-m cm))

(defn unwrap
  "Returns a [[ChoiceMap]] stripped of its wrappers."
  [m]
  (if (map? m)
    (update-vals m unwrap)
    m))

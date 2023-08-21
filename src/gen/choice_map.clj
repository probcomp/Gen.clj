(ns gen.choice-map
  "Choice map interface and data structure."
  (:import (clojure.lang
            Associative IFn IObj IPersistentMap IMapIterable MapEntry)))

(declare choice choice? choice-map? unwrap)

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
    (ChoiceMap. (.assoc ^IPersistentMap m k (choice v))))
  (without [_ k]
    (ChoiceMap. (.without ^IPersistentMap m k)))

  Associative
  (containsKey [_ k] (contains? m k))
  (entryAt [_ k]
    (when (contains? m k)
      (MapEntry/create k (unwrap (get m k)))))
  (cons [this o]
    (if (map? o)
      (reduce-kv assoc this o)
      (let [[k v] o]
        (ChoiceMap. (assoc m k (choice v))))))
  (count [_] (count m))
  (seq [_]
    (when-let [kvs (seq m)]
      (map (fn [[k v]]
             (MapEntry/create k (unwrap v)))
           kvs)))
  (empty [_] (ChoiceMap. (empty m)))
  (valAt [_ k]
    (unwrap (get m k)))
  (valAt [_ k not-found]
    (unwrap (get m k not-found)))
  (equiv [_ o]
    (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

  IMapIterable
  (keyIterator [_] (.keyIterator ^IMapIterable m))
  (valIterator [_]
    (.iterator ^Iterable (map unwrap (vals m))))

  Iterable
  (iterator [this] (.iterator ^Iterable (.seq this))))

(defmethod print-method ChoiceMap [^ChoiceMap cm ^java.io.Writer w]
  (.write w "#gen/choice-map ")
  (print-method (.-m cm) w))

;; ## Predicates

(defn choice? [x]
  (instance? Choice x))

(defn choice-map? [x]
  (instance? ChoiceMap x))

;; ## Constructors

(defn choice
  "Wraps `x` in a [[Choice]] instance."
  [x]
  (if (choice? x)
    x
    (->Choice x)))

(defn make
  "Returns a [[ChoiceMap]] built out of the supplied key-value pairs."
  [& {:as m}]
  (->ChoiceMap
   (update-vals m (fn [x]
                    (cond (or (choice? x)
                              (choice-map? x)) x
                          (map? x) (make x)
                          :else    (choice x))))))

(def EMPTY (->ChoiceMap {}))

;; ## API

(defn submaps
  "Unwrap a single layer of the [[ChoiceMap]] `cm`."
  [^ChoiceMap cm]
  (.-m cm))

(defn unwrap
  "If `m` is a [[Choice]] or [[ChoiceMap]], returns `m` stripped of its wrappers.
  Else, returns `m`"
  [m]
  (cond (choice? m) (:choice m)
        (map? m)    (update-vals m unwrap)
        :else m))

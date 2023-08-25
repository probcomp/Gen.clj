(ns gen.dynamic.choice-map
  (:require [gen.choice-map :as choice-map])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap
                            IMapIterable MapEntry))))

;; https://blog.wsscode.com/guide-to-custom-map-types/
;; https://github.com/originrose/lazy-map/blob/119dda207fef90c1e26e6c01aa63e6cfb45c1fa8/src/lazy_map/core.clj#L197-L278

(defrecord Choice [choice]
  choice-map/Value
  (value [_] choice))

#?(:clj
   (defmethod print-method Choice [choice ^java.io.Writer w]
     (.write w "#gen/choice ")
     (.write w (pr-str (choice-map/value choice)))))

(defn choice?
  "Returns `true` if `x` is an instance of `Choice`."
  [x]
  (instance? Choice x))

(defn choice
  "Creates a new leaf chioce map with `x` as its value."
  [x]
  (if (instance? Choice x)
    x
    (->Choice x)))

(declare choice-map choice-map? unwrap)

(defn auto-get-choice
  [x]
  (if (instance? Choice x)
    (choice-map/value x)
    x))

(deftype ChoiceMap [m]
  choice-map/Submaps
  (submaps [_] m)

  #?@(:cljs
      [Object
       (toString [this] (pr-str this))
       (equiv [this other] (-equiv this other))

       IPrintWithWriter
       (-pr-writer [cm writer _]
                   (write-all
                    writer
                    "#gen/choice-map "
                    (str (unwrap cm))))

       IFn
       (-invoke [_ k] (auto-get-choice (get m k)))

       IMeta
       (-meta [_] (-meta m))

       IWithMeta
       (-with-meta [_ meta-m] (ChoiceMap. (-with-meta m meta-m)))


       ICloneable
       (-clone [_] (ChoiceMap. (-clone m)))

       IIterable
       (-iterator [_] (-iterator m))

       ICollection
       (-conj [_ entry]
              (if (vector? entry)
                (ChoiceMap.
                 (-assoc m (-nth entry 0) (choice (-nth entry 1))))
                (ChoiceMap.
                 (reduce-kv (fn [acc k v]
                              (assoc acc k (choice v)))
                            m
                            entry))))

       IEmptyableCollection
       (-empty [_] (ChoiceMap. (-empty m)))

       IEquiv
       (-equiv [_ o] (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

       IHash
       (-hash [_] (-hash m))

       ISeqable
       (-seq [_]
             (when-let [kvs (seq m)]
               (map (fn [[k v]]
                      (MapEntry. k (auto-get-choice v) nil))
                    kvs)))

       ICounted
       (-count [_] (-count m))

       ILookup
       (-lookup [_ k] (auto-get-choice (-lookup m k)))
       (-lookup [_ k not-found]
                (let [v (-lookup m k ::not-found)]
                  (if (= v ::not-found)
                    not-found
                    (auto-get-choice v))))

       IAssociative
       (-assoc [_ k v] (ChoiceMap. (-assoc m k (choice v))))
       (-contains-key? [_ k] (-contains-key? m k))

       IFind
       (-find [_ k]
              (when-let [v (-find m k)]
                (MapEntry. (-key v) (auto-get-choice (-val v)) nil)))

       IMap
       (-dissoc [_ k] (ChoiceMap. (-dissoc m k)))

       IKVReduce
       (-kv-reduce [_ f init]
                   (-kv-reduce m
                               (fn [acc k v]
                                 (f acc k (auto-get-choice v)))
                               init))]

      :clj
      [Object
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
                  (MapEntry/create k (auto-get-choice (get m k)))))
       (cons [this o]
             (if (map? o)
               (reduce-kv assoc this o)
               (let [[k v] o]
                 (ChoiceMap. (assoc m k (choice v))))))
       (count [_] (count m))
       (seq [_]
            (when-let [kvs (seq m)]
              (map (fn [[k v]]
                     (MapEntry/create k (auto-get-choice v)))
                   kvs)))
       (empty [_] (ChoiceMap. (empty m)))
       (valAt [_ k]
              (auto-get-choice (get m k)))
       (valAt [_ k not-found]
              (auto-get-choice (get m k not-found)))
       (equiv [_ o]
              (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

       IMapIterable
       (keyIterator [_]
                    (.iterator ^Iterable (keys m)))
       (valIterator [_]
                    (.iterator ^Iterable (map auto-get-choice m)))

       Iterable
       (iterator [this] (.iterator ^Iterable (.seq this)))]))

(defn unwrap
  "If `m` is a [[Choice]] or [[ChoiceMap]], returns `m` stripped of its wrappers.
  Else, returns `m`"
  [m]
  (cond (choice? m) (:choice m)
        (map? m)    (update-vals m unwrap)
        :else m))

#?(:clj
   (defmethod print-method ChoiceMap [^ChoiceMap cm ^java.io.Writer w]
     (.write w "#gen/choice-map ")
     (print-method (unwrap cm) w)))

(defn choice-map
  [& {:as m}]
  (->ChoiceMap
   (update-vals m (fn [x]
                    (cond (or (instance? Choice x)
                              (instance? ChoiceMap x))
                          x

                          (map? x)
                          (choice-map x)

                          :else
                          (Choice. x))))))

(defn choice-map? [x]
  (instance? ChoiceMap x))

;; ## Reader literals

(defn ^:no-doc parse-choice
  "Implementation of a reader literal that turns literal forms into calls
  to [[choice]].

  Installed by default under `#gen/choice`."
  [form]
  `(choice ~form))

(defn ^:no-doc parse-choice-map
  "Implementation of a reader literal that turns literal map forms into calls
  to [[choice-map]].

  Installed by default under `#gen/choice-map`."
  [form]
  `(choice-map ~form))

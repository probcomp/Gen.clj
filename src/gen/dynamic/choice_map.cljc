(ns gen.dynamic.choice-map
  (:require [gen.choice-map :as cm])
  #?(:clj
     (:import (clojure.lang Associative IFn IObj IPersistentMap
                            IMapIterable MapEntry))))

;; https://www.gen.dev/docs/stable/ref/choice_maps/#Choice-Maps-1

;; Take implementation from Trie here...
;; https://github.com/flatland/ordered/blob/develop/src/flatland/ordered/map.clj

(declare unwrap unwrap choice)

;; TODO rename to `Value`... OR todo simply remove??
(defrecord Choice [choice])

(deftype ChoiceMap [m ks]
  cm/IChoiceMap
  (has-value? [m k]
    (instance? Choice (get m k)))

  ;; TODO note that get-value DOES do unwrapping. So when do we ever need wrapping?
  (get-value [m k]
    (when-let [v (get m k)]
      (when (instance? Choice v)
        (:choice v))))

  (get-submap [m k]
    (when-let [v (get m k)]
      (when (instance? ChoiceMap v)
        v)))

  (get-values-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (if (instance? Choice v)
          (assoc! acc k v)
          acc))
      (transient {})
      m)))

  (get-submaps-shallow [_]
    (persistent!
     (reduce-kv
      (fn [acc k v]
        (if (instance? ChoiceMap v)
          (assoc! acc k v)
          acc))
      (transient {})
      m)))

  (to-array [_]
    (into []
          (mapcat (fn [k]
                    (let [v (get m k)]
                      (if (instance? ChoiceMap v)
                        (cm/to-array v)
                        [v]))))
          ks))

  (-from-array [_ xs start-idx]
    (loop [i      0
           offset start-idx
           acc    {}]
      (if-let [k (nth ks i nil)]
        (let [v  (get m k)]
          (if (instance? ChoiceMap v)
            (let [[n ret] (cm/-from-array v xs i)]
              (recur (inc i)
                     (+ n offset)
                     (assoc acc k ret)))
            (recur (inc i)
                   (inc offset)
                   (assoc acc k (nth xs offset nil)))))
        [i acc])))

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
       (-invoke [_ k] (unwrap (get m k)))

       IMeta
       (-meta [_] (-meta m))

       IWithMeta
       (-with-meta [_ meta-m] (ChoiceMap. (-with-meta m meta-m) ks))


       ICloneable
       (-clone [_] (ChoiceMap. (-clone m) (-clone ks)))

       IIterable
       (-iterator [_] (-iterator m))

       ICollection
       (-conj [_ entry]
              (if (vector? entry)
                (ChoiceMap.
                 (-assoc m (-nth entry 0) (choice (-nth entry 1)))
                 ;; TODO fix!
                 ks)
                (ChoiceMap.
                 (reduce-kv (fn [acc k v]
                              (assoc acc k (choice v)))
                            m
                            entry)
                 ;; TODO FIX!
                 ks
                 )))

       IEmptyableCollection
       (-empty [_] (ChoiceMap. (-empty m) []))

       IEquiv
       (-equiv [_ o] (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

       IHash
       (-hash [_] (-hash m))

       ISeqable
       (-seq [_]
             (when-let [kvs (seq m)]
               (map (fn [[k v]]
                      (MapEntry. k (unwrap v) nil))
                    kvs)))

       ICounted
       (-count [_] (-count m))

       ILookup
       (-lookup [_ k] (unwrap (-lookup m k)))
       (-lookup [_ k not-found]
                (let [v (-lookup m k ::not-found)]
                  (if (= v ::not-found)
                    not-found
                    (unwrap v))))

       IAssociative
       (-assoc [_ k v] (ChoiceMap. (-assoc m k (choice v)) ks))
       (-contains-key? [_ k] (-contains-key? m k))

       IFind
       (-find [_ k]
              (when-let [v (-find m k)]
                (MapEntry. (-key v) (unwrap (-val v)) nil)))

       IMap
       (-dissoc [_ k] (ChoiceMap. (-dissoc m k) ks))

       IKVReduce
       (-kv-reduce [_ f init]
                   (-kv-reduce m
                               (fn [acc k v]
                                 (f acc k (unwrap v)))
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
       (withMeta [_ meta-m] (ChoiceMap. (with-meta m meta-m) ks))

       IPersistentMap
       (assocEx [_ _ _] (throw (Exception.)))
       (assoc   [_ k v]
                (ChoiceMap. (.assoc ^IPersistentMap m k (choice v))
                            (conj ks k)))
       (without [_ k]
                (ChoiceMap. (.without ^IPersistentMap m k)
                            ;; TODO error, implement!
                            ks))

       Associative
       (containsKey [_ k] (contains? m k))
       (entryAt [_ k]
                (when (contains? m k)
                  (MapEntry/create k (unwrap (get m k)))))
       (cons [this o]
             (if (map? o)
               (reduce-kv assoc this o)
               (let [[k v] o]
                 (ChoiceMap. (assoc m k (choice v))
                             ;; TODO error, implement!
                             ks))))
       (count [_] (count m))
       (seq [_]
            (when-let [kvs (seq m)]
              (map (fn [[k v]]
                     (MapEntry/create k (unwrap v)))
                   kvs)))
       (empty [_] (ChoiceMap. (empty m) []))
       (valAt [_ k]
              (unwrap (get m k)))
       (valAt [_ k not-found]
              (unwrap (get m k not-found)))
       (equiv [_ o]
              (and (instance? ChoiceMap o) (= m (.-m ^ChoiceMap o))))

       IMapIterable
       (keyIterator [_]
                    (.iterator ^Iterable (keys m)))
       (valIterator [_]
                    (.iterator ^Iterable (map unwrap m)))

       Iterable
       (iterator [this]
                 (if-let [xs (.seq this)]
                   (.iterator ^Iterable xs)
                   (.iterator {})))]))

#?(:clj
   (defmethod print-method Choice [choice ^java.io.Writer w]
     (.write w "#gen/choice ")
     (.write w (pr-str (:choice choice)))))

#?(:clj
   (defmethod print-method ChoiceMap [^ChoiceMap cm ^java.io.Writer w]
     (.write w "#gen/choice-map ")
     (print-method (unwrap cm) w)))

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


;; ## API

(defn choice?
  "Returns `true` if `x` is an instance of [[Choice]], false otherwise."
  [x]
  (instance? Choice x))

(defn choice-map?
  "Returns `true` if `x` is an instance of [[ChoiceMap]], false otherwise."
  [x]
  (instance? ChoiceMap x))

(defn choice
  "Creates a new leaf choice map with `x` as its value."
  [x]
  (if (instance? Choice x)
    x
    (->Choice x)))

(defn unwrap
  "If `m` is a [[Choice]] or [[ChoiceMap]], returns `m` stripped of its wrappers.
  Else, returns `m`"
  [m]
  (cond (choice? m) (:choice m)
        (map? m)    (update-vals m unwrap)
        :else m))

(defn choice-map
  [& {:as m}]
  (->ChoiceMap
   (update-vals m (fn [x]
                    (cond (or (choice? x)
                              (choice-map? x))
                          x

                          (map? x)
                          (choice-map x)

                          :else
                          (->Choice x))))
   (into [] (keys m))))

(ns gen.dynamic.choice-map
  (:import [clojure.lang MapEntry])
  (:require [gen.choice-map :as choice-map]))

(set! *warn-on-reflection* true)

;; https://blog.wsscode.com/guide-to-custom-map-types/
;; https://github.com/originrose/lazy-map/blob/119dda207fef90c1e26e6c01aa63e6cfb45c1fa8/src/lazy_map/core.clj#L197-L278

(defrecord Choice [choice]
  choice-map/Value
  (value [_] choice))

(defmethod print-method Choice [choice ^java.io.Writer w]
  (.write w "#gen/choice ")
  (.write w (pr-str (choice-map/value choice))))

(defn choice?
  "Returns `true` if `x` is an instance of `Choice`."
  [x]
  (instance? Choice x))

(defn choice
  "Creates a new leaf chioce map with `x` as its value."
  [x]
  (if (instance? Choice x)
    x
    (Choice. x)))

(declare choice-map choice-map?)

(defn auto-get-choice
  [x]
  (if (instance? Choice x)
    (choice-map/value x)
    x))

(deftype ChoiceMap [^clojure.lang.IPersistentMap m]
  clojure.lang.Associative
  (containsKey [_ k]
    (contains? m k))
  (entryAt [_ k]
    (when (contains? m k)
      (MapEntry/create k (auto-get-choice (get m k)))))


  clojure.lang.IFn
  (invoke [this k]
    (.valAt this k))

  clojure.lang.ILookup
  (valAt [_ k]
    (auto-get-choice (get m k)))
  (valAt [_ k not-found]
    (auto-get-choice (get m k not-found)))

  clojure.lang.IMapIterable
  (keyIterator [_]
    (.iterator
     ^java.lang.Iterable
     (keys m)))
  (valIterator [_]
    (.iterator
     ^java.lang.Iterable
     (map auto-get-choice m)))

  clojure.lang.IPersistentCollection
  (cons [this o]
    (if (map? o)
      (reduce-kv assoc this o)
      (let [[k v] o]
        (ChoiceMap. (.assoc m k (choice v))))))
  (count [_]
    (count m))
  (empty [_]
    (ChoiceMap. {}))
  (equiv [_ o]
    (and (instance? ChoiceMap o)
         (= m (.-m ^ChoiceMap o))))

  clojure.lang.IPersistentMap
  (assoc [_ k v]
    (ChoiceMap. (.assoc m k (choice v))))
  (without [_ k]
    (ChoiceMap. (.without m k)))

  clojure.lang.Seqable
  (seq [_]
    (when-let [kvs (seq m)]
      (map (fn [[k v]]
             (MapEntry/create k (auto-get-choice v)))
           kvs)))

  java.lang.Iterable
  (iterator [this]
    (.iterator
     ^java.lang.Iterable
     (.seq this)))

  java.lang.Object
  (toString [this]
    (pr-str this))

  choice-map/Submaps
  (submaps [_]
    m))


(defmethod print-method ChoiceMap [^ChoiceMap cm ^java.io.Writer w]
  (.write w "#gen/choice-map ")
  (let [print-inner-map (fn print-inner-map [m]
                          (.write w "{")
                          (doseq [[i [k v]] (map-indexed vector m)]
                            (.write w (pr-str k))
                            (.write w " ")
                            (cond (instance? ChoiceMap v)
                                  (print-inner-map (.-m ^ChoiceMap v))

                                  (instance? Choice v)
                                  (let [choice (choice-map/value v)]
                                    (if (map? choice)
                                      (.write w (pr-str v))
                                      (.write w (pr-str choice))))

                                  :else
                                  (throw (ex-info "Error printing choice map. Invalid choice map." {:parent cm :key k :value v})))
                            (when-not (= i (dec (count cm)))
                              (.write w " ")))
                          (.write w "}"))]
    (print-inner-map (.-m cm))))

(defn choice-map
  [& {:as m}]
  (-> m
      (update-vals (fn [x]
                     (cond (or (instance? Choice x)
                               (instance? ChoiceMap x))
                           x

                           (map? x)
                           (choice-map x)

                           :else
                           (Choice. x))))
      (ChoiceMap.)))

(defn choice-map?
  [x]
  (instance? ChoiceMap x))

(set! *data-readers* (assoc *data-readers* 'gen/choice choice))
(set! *data-readers* (assoc *data-readers* 'gen/choice-map choice-map))

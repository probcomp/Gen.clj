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

(defn auto-get-choice
  [x]
  (if (choice? x)
    (choice-map/value x)
    x))

(defn unwrap
  "If `m` is a [[Choice]] or [[ChoiceMap]], returns `m` stripped of its wrappers.
  Else, returns `m`"
  [m]
  (cond (choice? m) (:choice m)
        (map? m)    (update-vals m unwrap)
        :else m))

(defn choice-map
  [& {:as m}]
  (update-vals m (fn [x]
                   (cond (instance? Choice x)
                         x

                         (map? x)
                         (choice-map x)

                         :else
                         (Choice. x)))))

(defn choice-map? [m]
  (every? (fn [[_ v]]
            (or (choice? v)
                (choice-map? v)))
          m))

;; ## Reader literals

(defn ^:no-doc parse-choice
  "Implementation of a reader literal that turns literal forms into calls
  to [[choice]].

  Installed by default under `#gen/choice`."
  [form]
  `(choice ~form))

(ns gen.choice-map
  "Choice map interface and data structure.")

(defrecord Choice [choice])

;; ## Predicates

(defn choice? [x]
  (instance? Choice x))

;; ## Constructors

(defn choice
  "Wraps `x` in a [[Choice]] instance."
  [x]
  (if (choice? x)
    x
    (->Choice x)))

(defn make
  "Returns a choice map built out of the supplied key-value pairs."
  [& {:as m}]
  (update-vals m (fn [x]
                   (cond (choice? x) x
                         (map? x)    (make x)
                         :else       (choice x)))))

;; ## API

(defn unwrap
  "If `m` is a [[Choice]] or [[ChoiceMap]], returns `m` stripped of its wrappers.
  Else, returns `m`"
  [m]
  (cond (choice? m) (:choice m)
        (map? m)    (update-vals m unwrap)
        :else m))

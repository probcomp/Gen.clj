(ns gen.dynamic.choice-map)

;; [x] Gen.get_value — Function. ; (get (values cm) k)
;; [x] Gen.has_value — Function. ; (contains? (values cm) k)
;; [x] Gen.get_submap — Function. ; (get (submaps cm) k)
;; [x] Gen.get_values_shallow — Function. ; (values cm)
;; [x] Gen.get_value — Function. (get (values cm) k)
;; [x] Gen.get_submaps_shallow — Function. ; (submaps cm)
;; [x] Gen.to_array — Function. ; (into [] (values cm))
;; [ ] Gen.from_array — Function.
;; [ ] Gen.get_selected — Function.

(defrecord Choice [choice])

(defn value
  "Returns the current choice TODO complete!"
  [choice]
  (when (instance? Choice choice)
    (:choice choice)))

#?(:clj
   (defmethod print-method Choice [choice ^java.io.Writer w]
     (.write w "#gen/choice ")
     (.write w (pr-str (value choice)))))

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
    (value x)
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

;; TODO can we kill this? Why is this here?
(def submaps choice-map)

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

(ns gen.choice-map
  "Protocol defining the choice map interface."
  (:refer-clojure :exclude [to-array]))

(defprotocol IChoiceMap
  (has-value? [m k])
  (get-value [m k] "NOTE that this returns a value, not a submap.")
  (get-submap [m k] "NOTE that this returns a submap, not a value.")
  (get-values-shallow [m] "Return just values")
  (get-submaps-shallow [m] "Returns just submaps")
  (to-array [m])
  (-from-array [m a i]))

(defn from-array [m xs]
  (let [[n ret] (-from-array m xs 0)]
    (if (= n (count xs))
      ret
      (throw (ex-info "Dimension mismatch: " {:xs xs :n n})))))

(defrecord EmptyChoiceMap []
  IChoiceMap
  (has-value? [_ _] false)
  (get-value [_ _] nil)
  (get-submap [this _] this)
  (get-values-shallow [_] {})
  (get-submaps-shallow [_] {})
  (to-array [_] [])
  (-from-array [this _ _] [0 this]))

(def EMPTY
  "Empty choicemap singleton."
  (->EmptyChoiceMap))

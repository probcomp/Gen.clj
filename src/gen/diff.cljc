(ns gen.diff)

(defrecord NoChange [])

(def no-change (->NoChange))

(defrecord UnknownChange [])

(def unknown-change (->UnknownChange))

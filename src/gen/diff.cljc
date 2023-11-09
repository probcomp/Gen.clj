(ns gen.diff)

(defprotocol IDiff)

(defn diff? [x]
  (satisfies? IDiff x))

(defrecord NoChange []
  IDiff)

(defrecord UnknownChange []
  IDiff)

(def no-change (NoChange.))
(def unknown-change (UnknownChange.))

(defrecord SetDiff [added deleted]
  IDiff)

(defrecord DictDiff [added deleted updated]
  IDiff)

(defrecord VectorDiff [new-length prev-length updated]
  IDiff)

(ns gen.diff
  "Implementations of the [[IDiff]] protocol, for recording distinct differences
  between Clojure values seen by a function at different invocations.")

;; ## IDiff
;;
;; [[IDiff]] This is a marker protocol, used by types that record some diff to a
;; Clojure data type.

(defprotocol IDiff)

(defn diff?
  "Returns true if `x` satisfies [[IDiff]], false otherwise."
  [x]
  (satisfies? IDiff x))

;; ### Implementations

(defrecord UnknownChange []
  IDiff)

(def unknown-change
  "Used to note that no information is provided about the change to the value."
  (UnknownChange.))

(defrecord NoChange []
  IDiff)

(def no-change
  "The value definitely did not change."
  (NoChange.))

;; Composite of
;;
;; - a set of elements added
;; - a set of elements deleted

(defrecord SetDiff [added deleted]
  IDiff)

;; Composite of
;;
;; - a map of entries added
;; - a set of keys deleted
;; - a map of key => diff value for the key

(defrecord DictDiff [added deleted updated]
  IDiff)

;; Composite of

;; - new length
;; - previous length
;; - a map of updated index => diff..

(defrecord VectorDiff [new-length prev-length updated]
  IDiff)

;; ## IDiffed
;;
;; Extend this protocol to wrapper types used for composing a value with
;; information about a change to the value.

(defprotocol IDiffed
  (get-diff [x]
    "If `x` is an instance of [[IDiffed]], returns the attached [[IDiff]]
  instance. Else, acts as identity.")
  (strip-diff [x]
    "If `x` is an instance of [[IDiffed]], returns the wrapped value. Else, acts
  as identity."))

(extend-protocol IDiffed
  #?(:clj Object :cljs default)
  (get-diff [_] no-change)
  (strip-diff [x] x))

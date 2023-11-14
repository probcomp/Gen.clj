(ns gen.array
  "Protocol and API methods for converting types to and from an array of values.

  These functions are useful for converting nested, hierarchical data structures
  into ordered vectors without structure and recovering them back."
  (:refer-clojure :exclude [to-array]))

(defprotocol IArray
  (to-array [x] "Returns a flat vector representation of `x`.")
  (-from-array [x a start-idx]
    "Given some `x`, an input vector `a` and a starting index `start-idx`,
    returns a pair of

  - the number of elements read off of the vector
  - an instance of the same type and shape as `x`, with its leaves populated by
    values from `a` beginning at `start-idx`."))

(defn from-array
  "Given some structured input `m` and an input vector `xs`, returns an instance
  of the same type and shape as `m`, with its leaves populated by values from
  `xs`.

  Throws an exception if the count of `xs` doesn't match the number of leaves in
  `m`."
  [m xs]
  (let [[n ret] (-from-array m xs 0)]
    (if (= n (count xs))
      ret
      (throw (ex-info "Dimension mismatch: " {:xs xs :n n})))))

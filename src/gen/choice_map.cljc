(ns gen.choice-map
  "Protocols that constitute the choice map interface.")

;; https://www.gen.dev/docs/stable/ref/choice_maps/#Choice-Maps-1

;; [x] Gen.get_value — Function. ; (get (values cm) k)
;; [x] Gen.has_value — Function. ; (contains? (values cm) k)
;; [x] Gen.get_submap — Function. ; (get (submaps cm) k)
;; [x] Gen.get_values_shallow — Function. ; (values cm)
;; [x] Gen.get_value — Function. (get (values cm) k)
;; [x] Gen.get_submaps_shallow — Function. ; (submaps cm)
;; [x] Gen.to_array — Function. ; (into [] (values cm))
;; [ ] Gen.from_array — Function.
;; [ ] Gen.get_selected — Function.

(defprotocol Value
  :extend-via-metadata true
  (value [cm] "Returns the value for "))

(defprotocol Values
  :extend-via-metadata true
  (values [cm] "Returns an associative data structure mapping keys to values."))

(defprotocol Submaps
  :extend-via-metadata true
  (submaps [cm] "Returns an associative data structure mapping keys to submaps."))

(defprotocol Leaf
  :extend-via-metadata true
  (leaf-value [cm] "Returns the value for"))

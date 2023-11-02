(ns gen.choice-map-test)

#_(let [m (->SortedMapChoiceMap
           (sorted-map :d 'a :a (->SortedMapChoiceMap
                                 (sorted-map :d 'a :a 'b :c" d")) :c" d"))]
    (from-array m (to-array m)))

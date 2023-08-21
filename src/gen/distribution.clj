(ns gen.distribution
  (:require [gen.choice-map :as choice-map]
            [gen.generative-function :as gf]
            [gen.dynamic.trace :as trace]))

(defn dist->gen-fn
  [& {:keys [ctor sample-fn score-fn encode decode]
      :or {encode identity
           decode identity}}]
  (let [args->sample (fn [& args]
                       (sample-fn (apply ctor args)))]
    (with-meta (comp decode args->sample)
      {`gf/simulate
       (fn [gf args]
         (let [dist   (apply ctor args)
               sample (sample-fn dist)
               score  (score-fn dist sample)
               val    (decode sample)]
           (trace/->PrimitiveTrace gf args val score)))
       `gf/generate
       (fn
         ([gf args]
          {:weight 0.0
           :trace  (gf/simulate gf args)})
         ([gf args constraint]
          (assert (choice-map/choice? constraint))
          (let [dist   (apply ctor args)
                val    (choice-map/unwrap constraint)
                weight (score-fn dist (encode val))]
            {:weight weight
             :trace  (trace/->PrimitiveTrace gf args val weight)})))})))

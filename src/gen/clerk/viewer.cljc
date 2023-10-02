(ns gen.clerk.viewer
  (:refer-clojure :exclude [delay future])
  (:require #?(:clj [clojure.core :as core])
            [nextjournal.clerk.viewer :as viewer]))

(defn sleep [ms]
  #?(:clj  (Thread/sleep ms)
     :cljs (js/setTimeout (fn []) ms)))

(def delay
  {:var-from-def? true
   :transform-fn
   (comp viewer/mark-presented
         (viewer/update-val
          (fn [{:nextjournal.clerk/keys [var-from-def]}]
            (assert (var? var-from-def))
            (let [var-symbol (symbol var-from-def)
                  delay @var-from-def
                  realized (realized? delay)]
              (cond-> {:var-symbol var-symbol
                       :delay (viewer/present delay)
                       :realized realized}
                realized (assoc :val (viewer/present @delay)))))))

   :render-fn
   '(fn [{:keys [realized delay var-symbol val]}]
      (if realized
        [nextjournal.clerk.render/inspect-presented val]
        (reagent.core/with-let [!evaluating (reagent.core/atom false)]
          [:<>
           (if @!evaluating
             [:button {:type "button"
                       :class "bg-neutral-700 dark:bg-neutral-500 text-white dark:text-black font-bold font-sans text-xs py-1 px-2 mr-2 rounded"
                       :disabled true}
              "forcing..."]
             [:button {:type "button"
                       :class "bg-blue-500 hover:bg-blue-700 text-white dark:text-black font-bold font-sans text-xs py-1 px-2 mr-2 rounded"
                       :on-click #(when-not @!evaluating
                                    (reset! !evaluating true)
                                    (nextjournal.clerk.viewer/clerk-eval
                                     `(force ~var-symbol)))}
              "force"])
           [nextjournal.clerk.render/inspect-presented delay]])))})

#?(:clj
   (def future
     {:var-from-def? true
      :transform-fn
      (comp viewer/mark-presented
            (viewer/update-val
             (fn [{:nextjournal.clerk/keys [var-from-def]}]
               (assert (var? var-from-def))
               (let [var-symbol (symbol var-from-def)
                     future @var-from-def
                     cancelled (future-cancelled? future)
                     done (future-done? future)]
                 (cond-> {:var-symbol var-symbol
                          :future (viewer/present future)
                          :done done
                          :cancelled cancelled}
                   (and (not cancelled) done)
                   (assoc :val (viewer/present @future)))))))

      :render-fn
      '(fn [{:keys [future var-symbol val done cancelled]}]
         (cond cancelled
               [:button {:type "button"
                         :class "bg-blue-500 hover:bg-blue-700 text-white dark:text-black font-bold font-sans text-xs py-1 px-2 mr-2 rounded"
                         :on-click #(nextjournal.clerk.viewer/clerk-eval
                                     `(nextjournal.clerk/clear-cache! '~var-symbol))}
                "restart"]

               done
               [nextjournal.clerk.render/inspect-presented val]

               :else
               [:<>
                [:button {:type "button"
                          :class "bg-neutral-700 dark:bg-neutral-500 text-white dark:text-black font-bold font-sans text-xs py-1 px-2 mr-2 rounded"
                          :on-click #(nextjournal.clerk.viewer/clerk-eval
                                      `(future-cancel ~var-symbol))}
                 "cancel"]
                [nextjournal.clerk.viewer/inspect-presented future]]))}))

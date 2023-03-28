(ns gen.clerk.callout
  (:require [nextjournal.clerk :as clerk]))

(defn alert
  [& {:keys [title message color] :or {color :orange}}]
  (with-meta
    (clerk/html
     [:div {:class (str
                    (case (name color)
                      "green" "bg-green-100 border-green-500 text-green-700"
                      "orange" "bg-orange-100 border-orange-500 text-orange-700"
                      "red" "bg-red-100 border-red-500 text-red-700")
                    " font-sans border-l-4 p-4")
            :role "alert"}
      [:div {:class "font-bold"} title]
      (clerk/md message)])
    {::clerk/visibility {:code :hide}}))

(defn hint
  [message & {:as m}]
  (alert (assoc m
                :message message
                :title "Hint"
                :color "green")))

(defn warning
  [message & {:as m}]
  (alert (assoc m
                :message message
                :title "Warning"
                :color "orange")))

(defn caveat
  [message & {:as m}]
  (alert (assoc m
                :message message
                :title "Caveat"
                :color "orange")))

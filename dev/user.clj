(ns user
  (:require [nextjournal.clerk :as clerk]))

(def index
  "examples/introduction.clj")

(def notebooks
  ["examples/introduction.clj"
   "examples/intro_to_modeling.clj"])

(def defaults
  {:index index})

(def serve-defaults
  (assoc defaults
         :browse? true
         :watch-paths ["examples"]))

(def static-defaults
  (assoc defaults
         :browse? false
         :paths notebooks
         :git/url "https://github.com/InferenceQL/gen.clj"))

(defn serve!
  ([] (serve! {}))
  ([opts]
   (let [{:keys [browse? index] :as opts} (merge serve-defaults opts)]
     (when (and browse? index)
       (clerk/show! index))
     (clerk/serve! opts))))

(def halt! clerk/halt!)

(defn build!
  ([] (build! {}))
  ([opts]
   (clerk/build!
    (merge static-defaults opts))))

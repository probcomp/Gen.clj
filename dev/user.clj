(ns user
  (:require [gen.clerk :as clerk]))

(set! *warn-on-reflection* true)

(def index
  "examples/intro_to_modeling.clj")

(def notebooks
  ["examples/introduction.clj"
   "examples/intro_to_modeling.clj"
   "examples/intro_to_modeling/edit.clj"])

(def defaults
  {:index index
   ;; Enable / uncomment this when working on new components.
   ;; :cljs-namespaces '[gen.sci-extensions]
   })

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
   (clerk/serve!
    (merge serve-defaults opts))))

(def halt! clerk/halt!)

(defn build! [opts]
  (clerk/build!
   (merge static-defaults opts)))

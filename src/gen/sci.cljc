(ns gen.sci
  "Functions for installation of all namespaces into an SCI context."
  (:require [gen.array]
            [gen.choicemap]
            [gen.clerk.callout]
            [gen.clerk.viewer]
            [gen.diff]
            [gen.distribution]
            [gen.distribution.kixi]
            [gen.distribution.math.log-likelihood]
            [gen.dynamic]
            [gen.generative-function]
            [gen.inference.importance]
            [gen.trace]
            [sci.core :as sci]
            [sci.ctx-store]))

(def ^:no-doc gen-macro ^:sci/macro
  (fn [_&form _&env & args]
    (apply gen.dynamic/gen-body args)))

(def ^:no-doc untraced-macro ^:sci/macro
  (fn [_&form _&env & body]
    `(binding [gen.dynamic/*trace* no-op]
       ~@body)))

(def namespaces
  {'gen.array                            (sci/copy-ns gen.array (sci/create-ns 'gen.array))
   'gen.choicemap                        (sci/copy-ns gen.choicemap (sci/create-ns 'gen.choicemap))
   'gen.clerk.callout                    (sci/copy-ns gen.clerk.callout (sci/create-ns 'gen.clerk.callout))
   'gen.clerk.viewer                     (sci/copy-ns gen.clerk.viewer (sci/create-ns 'gen.clerk.viewer))
   'gen.diff                             (sci/copy-ns gen.diff (sci/create-ns 'gen.diff))
   'gen.distribution                     (sci/copy-ns gen.distribution (sci/create-ns 'gen.distribution))
   'gen.distribution.kixi                (sci/copy-ns gen.distribution.kixi (sci/create-ns 'gen.distribution.kixi))
   'gen.distribution.math.log-likelihood (sci/copy-ns gen.distribution.math.log-likelihood (sci/create-ns 'gen.distribution.math.log-likelihood))
   'gen.dynamic                          (-> (sci/copy-ns gen.dynamic (sci/create-ns 'gen.dynamic))
                                             (assoc 'gen gen-macro
                                                    'untraced untraced-macro))
   'gen.generative-function              (sci/copy-ns gen.generative-function (sci/create-ns 'gen.generative-function))
   'gen.inference.importance             (sci/copy-ns gen.inference.importance (sci/create-ns 'gen.inference.importance))
   'gen.trace                            (sci/copy-ns gen.trace (sci/create-ns 'gen.trace))})

(def config
  "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate Emmy forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."
  {:namespaces namespaces
   :classes #?(:clj  {'java.lang.Math java.lang.Math}
               :cljs {'Math js/Math})})

(def context
  "sci context required to evaluate Gen.clj forms via SCI."
  (sci/init config))

(defn install!
  "Installs [[config]] into the shared SCI context store."
  []
  (sci.ctx-store/swap-ctx!
   sci/merge-opts
   config))

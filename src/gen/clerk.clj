(ns gen.clerk
  "Clerk-specific utilities for Gen.clj.

  Use [[serve!]], [[halt!]] and [[build!]] in place of the Clerk versions."
  {:nextjournal.clerk/toc true}
  (:require [mentat.clerk-utils.build :as b]))

(def custom-js
  "CDN address of a pre-built JS bundle for Clerk with support for all of this
  library's viewers."
  "https://cas.clerk.garden/tree/8VxCcd2nQqrhPLVttyyM5Rnv8qMYcjeSEjgJnhZTvCgVhmgsNWWeusxmVkfX9ajrXmFqQEdVmtdHSoCdHLG2XTsXXB/.clerk/shadow-cljs/main.js")

;; ## Viewers
;;
;; This first section contains Clerk viewer implementations that are helpful for
;; presentation or make the rest of the code here possible.


;; ### Project Configuration

(defn serve!
  "Version of [[nextjournal.clerk/serve!]] that swaps out the default JS bundle
  for a custom Gen.clj bundle.

  In addition to all options supported by Clerk's `serve!`, [[serve!]] supports
  the following options:

  - `:cljs-namespaces`: a sequence of CLJS namespaces to compile and make
    available to Clerk. If provided, [[serve!]] will compile a custom CLJS bundle
    and configure Clerk to use this bundle instead of the Gen.clj bundle.

  - `:custom-js`: custom JS bundle to use instead of Gen.clj's JS.

  - `:shadow-options`: these options are forwarded
    to [[mentat.clerk-utils.build.shadow/watch!]]. See that function's docs for
    more detail.

    This bundle is served from a running shadow-cljs server and recompiled when
    any dependency or namespace changes. Defaults to `nil`.

  The only other difference is that if `(:browse? opts)` is `true`, [[serve!]]
  calls [[nextjournal.clerk/show!]] automatically on `(:index opts)` if
  provided.

  All remaining `opts` are forwarded to [[nextjournal.clerk/serve!]]."
  ([] (serve! {}))
  ([opts]
   (let [opts (if (or (:cljs-namespaces opts)
                      (:custom-js opts))
                opts
                (assoc opts :custom-js custom-js))]
     (b/serve! opts))))

(defn halt!
  "Version of [[nextjournal.clerk/halt!]] that additionally kills any shadow-cljs
  processes, if they are running, and resets all custom CSS entries."
  []
  (b/halt!))

(defn build!
  "Version of [[nextjournal.clerk/build!]] that swaps out the default JS bundle
  for a custom Gen.clj bundle.

  In addition to all options supported by Clerk's `build!`, [[build!]] supports
  the following options:

  - `:cljs-namespaces`: a sequence of CLJS namespaces to compile and make
    available to Clerk. If provided, [[build!]] will compile a custom CLJS bundle
    and configure Clerk to use this bundle instead of its default. Defaults to
    `nil`.

  - `:custom-js`: custom JS bundle to use instead of Gen.clj's built-in JS.

  - `:cname`: string denoting the custom hostname from which the site will be
    served. If provided, [[build!]] will create a `CNAME` file containing the
    value in `(:out-path opts)`. Defaults to `nil`.

  The only other difference is that [[build!]] populates `:git/sha` if it hasn't
  been provided.

  All remaining `opts` are forwarded to [[nextjournal.clerk/build!]]"
  [opts]
  (let [opts (if (or (:cljs-namespaces opts)
                     (:custom-js opts))
               opts
               (assoc opts :custom-js custom-js))]
    (b/build! opts)))

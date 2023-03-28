(ns gen.reducers
  (:refer-clojure :exclude [repeatedly])
  (:require [clojure.core :as clojure]
            [clojure.core.reducers :as reducers]))

(declare fold-repeatedly)

(deftype Repeatedly [inv f]
  clojure.core.reducers/CollFold
  (coll-fold [_ n combinef reducef]
    (fold-repeatedly inv f n combinef reducef))

  clojure.lang.IReduce
  (reduce [_ rf]
    (if (pos? inv)
      (.reduce (Repeatedly. (dec inv) f)
               rf
               (f))
      (rf)))
  (reduce [_ rf x]
    (loop [ret x
           inv inv]
      (if (pos? inv)
        (let [ret (rf ret (f))]
          (if (reduced? ret)
            @ret
            (recur ret (dec inv))))
        ret))))

(defn repeatedly
  "Like `clojure.core/repeatedly`, but is reducible and foldable."
  [n f]
  (Repeatedly. n f))

(def fjfork @#'reducers/fjfork)
(def fjjoin @#'reducers/fjjoin)
(def fjtask @#'reducers/fjtask)
(def fjinvoke @#'reducers/fjinvoke)

(defn- fold-repeatedly
  [inv f n combinef reducef]
  (cond (< inv 1) (combinef)

        (<= inv n)
        (reduce reducef (combinef) (Repeatedly. inv f))

        :else
        (let [inv1 (quot inv 2)
              inv2 (+ inv1 (mod inv 2))
              fc (fn [inv]
                   #(fold-repeatedly inv f n combinef reducef))]
          (fjinvoke
           #(let [f1 (fc inv1)
                  t2 (fjtask (fc inv2))]
              (fjfork t2)
              (combinef (f1) (fjjoin t2)))))))

(comment

  (time (into [] (clojure/repeatedly 1000000 rand)))
  (time (into [] (repeatedly 1000000 rand)))

  (time (reduce + (clojure/repeatedly 1000000 rand)))
  (time (reduce + (repeatedly 1000000 rand)))

  (time (reducers/fold + (clojure/repeatedly 1000000 rand)))
  (time (reducers/fold + (repeatedly 1000000 rand)))

  (time (reducers/fold 10 + + (clojure/repeatedly 1000000 rand)))
  (time (reducers/fold 10 + + (repeatedly 1000000 rand)))

  (time (reducers/fold 2 + + (clojure/repeatedly 1000000 rand)))
  (time (reducers/fold 2 + + (repeatedly 1000000 rand)))

  (time (reducers/fold 1 + + (clojure/repeatedly 1000000 rand)))
  (time (reducers/fold 1 + + (repeatedly 1000000 rand)))

  ;;

  (def slow-rand #(do (Thread/sleep 0 1) (rand)))

  (time (reduce + (clojure/repeatedly 1000 slow-rand)))
  (time (reduce + (repeatedly 1000 slow-rand)))

  (time (reducers/fold + (clojure/repeatedly 1000 slow-rand)))
  (time (reducers/fold + (repeatedly 1000 slow-rand)))

  (time (reducers/fold 10 + + (clojure/repeatedly 1000 slow-rand)))
  (time (reducers/fold 10 + + (repeatedly 1000 slow-rand)))

  (time (reducers/fold 2 + + (clojure/repeatedly 1000 slow-rand)))
  (time (reducers/fold 2 + + (repeatedly 1000 slow-rand)))

  (time (reducers/fold 1 + + (clojure/repeatedly 1000 slow-rand)))
  (time (reducers/fold 1 + + (repeatedly 1000 slow-rand)))

  (set! *print-length* 10)

  ,)

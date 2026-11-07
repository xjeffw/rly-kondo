(ns app.shadows
  (:require [lib.base :refer [base-fn]]
            [lib.middle :refer [middle-fn]]))

;; an own definition, a referral and a local come before a global
(defn base-value [x]
  x)

(defn shadowed [late-fn x]
  (middle-fn x)
  (base-fn x)
  (base-value x)
  (late-fn x))

(ns lib.middle
  (:require [lib.base :as base]))

(defn ^:global middle-fn [x]
  (base/base-fn x))

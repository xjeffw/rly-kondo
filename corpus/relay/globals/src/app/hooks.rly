(ns app.hooks
  (:require [lib.hooked :as hooked]))

(defn hooked [x]
  (with-binding [a x]
    a)
  (with-renamed-binding [b x]
    b))

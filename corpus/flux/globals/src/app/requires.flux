(ns app.requires
  (:require [lib.middle :as middle]
            [lib.outside :as outside]))

;; lib.middle holds lib.base, whose global this uses: the require is in use. Nothing
;; uses lib.outside.
(defn reached [x]
  (base-fn x))

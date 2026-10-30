(ns lib.hooked)

;; a macro whose hook is why its arguments are not names of vars
(defmacro ^:global with-binding [binding & body]
  `(let ~binding ~@body))

(defmacro ^{:global with-renamed-binding} renamed-binding [binding & body]
  `(let ~binding ~@body))

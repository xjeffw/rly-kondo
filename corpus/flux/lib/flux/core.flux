(ns flux.core)

(defmacro ^{:definition-symbol {:path [0] :type :variable}} defstate [var value]
  `(defonce ~var ~value))

(defmacro ^{:definition-symbol {:path [0] :type :variable}} defalias [name target]
  `(alias-var! '~name (var ~target)))

(defmacro ^{:definition-symbol {:path [0 1] :type :function}} defhandler [[kind name] value]
  `(def ~name [~kind ~value]))

(defmacro ignore-errors [& body]
  `(try
     ~@body
     (catch exception _ nil)))

(defn keepv [f coll]
  (f coll))

(defn- private-helper [x]
  x)

(defn uses-private-helper [x]
  (private-helper x))

(defstate *buffer* nil)

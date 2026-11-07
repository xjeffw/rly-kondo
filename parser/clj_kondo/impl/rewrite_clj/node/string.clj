(ns ^{:no-doc true} clj-kondo.impl.rewrite-clj.node.string
  (:require [clj-kondo.impl.rewrite-clj.node.protocols :as node]
            [clj-kondo.impl.toolsreader.v1v2v2.clojure.tools.reader.edn :as edn]
            [clojure.string :as string]))

;; ## Node

(defn- wrap-string
  [s]
  (str "\"" s "\""))

(defn- join-lines
  [lines]
  (string/join "\n" lines))

(def ^:dynamic *unescape*
  "When bound, the function that gives the value of the text of a string
  literal, in place of Clojure's rules: Relay Lisp has its own."
  nil)

(defn- maybe-unescape
  [s]
  (if (string/includes? s "\\")
    (if-let [unescape *unescape*]
      (unescape s)
      (edn/read-string (wrap-string s)))
    s))

(defrecord StringNode [lines]
  node/Node
  (tag [_]
    (if (next lines)
      :multi-line
      :token))
  (printable-only? [_]
    false)
  (sexpr [_]
    (if (= (count lines) 1)
      (maybe-unescape (nth lines 0))
      (join-lines (map maybe-unescape lines))))
  (length [_]
    (+ 2 (reduce + (map count lines))))
  (string [_]
    (wrap-string (join-lines lines)))

  Object
  (toString [this]
    (node/string this)))

(node/make-printable! StringNode)

;; ## Constructors

(defn string-node
  "Create node representing a string value.
   Takes either a seq of strings or a single one."
  [lines]
  (if (string? lines)
    (->StringNode [lines])
    (->StringNode lines)))

(ns clj-kondo.impl.relay
  "Relay Lisp, the language of `.rly` files: where it differs from Clojure.

  A Relay file is analyzed as Clojure (`:lang :clj`) with `:relay?` set in the
  ctx and in each namespace it declares. The differences:

  - Every namespace refers the public vars of `relay.core` and `relay.builtins`,
    as if by `(:use relay.core relay.builtins)`, minus the names its
    `:refer-clojure` clause excludes. These two namespaces take the place of
    `clojure.core`: an unqualified name is a var of them or it does not resolve.
    Relay declares its special forms as vars of `relay.core` too.
  - A qualified name resolves in any loaded namespace, required or not.
  - `^:private` keeps a var out of what other namespaces refer implicitly, and
    nothing else.
  - The error type of a `catch` clause is a name that is never evaluated.
  - A string keeps an escape the reader does not know, backslash included.
  - A vector is called with an index and, optionally, a default.
  - conj! and its like change their collection in place.
  - A file is named after its namespace as written, hyphens included, and a
    package keeps the namespace `P.tests.X` in `test/P/X.rly`. A `package.rly`
    is the description of a package: data, not code.
  - A macro says which of its arguments names the var it defines, in its
    `:definition-symbol` metadata.
  - A var with `:global` metadata is a global: a namespace whose dependency tree
    holds the namespace that registered it names it by its plain symbol, after
    everything else that name could be, and by `global/name`. A namespace's tree
    is what its `ns` form and the `require` and `use` forms at the top level of
    its file name, transitively. `user` and the files of the user's configuration
    see every global."
  {:no-doc true}
  (:require [clj-kondo.impl.var-info :as var-info]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def core-ns 'relay.core)

(def builtins-ns 'relay.builtins)

(def implicit-namespaces
  "The namespaces every Relay namespace refers, in the order they are searched."
  [core-ns builtins-ns])

(def global-ns
  "The pseudo-namespace that every Relay namespace uses implicitly: `global/name`
  names the global `name`. No file defines it."
  'global)

(def ^:private user-ns
  "The namespace of interactive code, and of a file without an `ns` form."
  'user)

(def ^:private library-ns
  "The namespace that requires the library, which every user namespace depends
  on."
  'relay.lib)

(defn relay-file? [filename]
  (and (string? filename)
       (str/ends-with? filename ".rly")))

(defn package-description?
  "True for the file that describes a Relay package, a `package.rly` that holds
  one map: data, where a symbol names a namespace and not a var. `forms` are
  the parsed top level forms of the file."
  [filename forms]
  (and (string? filename)
       (some? (re-find #"(^|[/\\\\])package\.rly$" filename))
       (= 1 (count forms))
       (identical? :map (:tag (first forms)))))

(defn analysis-lang
  "The language to analyze the parsed file `parsed` as: `lang`, or EDN for the
  description of a Relay package."
  [ctx filename parsed lang]
  (if (and (:relay? ctx)
           (package-description? filename (:children parsed)))
    :edn
    lang))

(defn file-names-namespace?
  "True when the file at `path`, given without its extension and with dots for
  its separators, is where Relay looks for the namespace `ns-name`: under the
  name of the namespace or, in a package, under `test` for a namespace of its
  tests and under `src` for one that the `relay` prefix was added to."
  [path ns-name]
  (let [parts (str/split (str ns-name) #"\.")
        relay? (= "relay" (first parts))
        suffix (fn [dir parts] (str dir "." (str/join "." parts)))]
    (boolean
     (some #(str/ends-with? path %)
           (cond-> [(str ns-name)]
             (and (<= 3 (count parts)) (= "tests" (second parts)))
             (conj (suffix "test" (cons (first parts) (drop 2 parts))))
             (and relay? (<= 4 (count parts)) (= "tests" (second parts)))
             (conj (suffix "test" (drop 2 parts)))
             (and relay? (<= 3 (count parts)))
             (conj (suffix "src" (rest parts))))))))

(defn implicit-refer-alls
  "The :refer-alls entries of the namespaces that the Relay namespace `ns-name`
  refers without saying so."
  [ns-name excluded filename]
  (into {}
        (comp (remove #{ns-name})
              (map (fn [implicit-ns]
                     [implicit-ns {:excluded (or excluded #{})
                                   :implicit true
                                   :referred #{}
                                   :filename filename}])))
        implicit-namespaces))

(defn dependencies
  "The namespaces that a namespace depends on through the required namespaces
  `required`, which clj-kondo keeps with what their libspecs say: all but those
  that a libspec only gives an `:as-alias`."
  [required]
  (into #{}
        (comp (remove #(-> % meta :alias meta :as-alias))
              (map #(with-meta % nil)))
        required))

(defn top-level-require?
  "True when `ctx` is that of a `require` or `use` form at the top level of a
  Relay file, which is what the loader reads a namespace's dependencies from: a
  form inside another form is not, nor is the libspec of `after!`."
  [ctx]
  (let [[[_ called] & enclosing] (:callstack ctx)]
    (boolean (and (:relay? ctx)
                  (not enclosing)
                  (contains? '#{require use} called)))))

(defn with-implicit-refers
  "Marks the namespace map `ns` as a Relay namespace and adds what it refers
  implicitly."
  [ns filename]
  (let [refer-alls (implicit-refer-alls (:name ns) (:clojure-excluded ns) filename)]
    (-> ns
        (assoc :relay? true)
        ;; always loaded, so their qualified names need no require
        (update :qualify-ns #(reduce (fn [m n] (assoc m n n))
                                     %
                                     (conj implicit-namespaces global-ns)))
        (update :refer-alls #(merge refer-alls %))
        (update :used-namespaces (fnil into #{}) (keys refer-alls)))))

(defn with-dependencies
  "Records in the namespace map `ns` the namespaces of `required`, what its `ns`
  form requires in the file whose first namespace is `top-ns`."
  [ns top-ns required]
  (assoc-in ns [:relay-dependencies (or top-ns (:name ns))] (dependencies required)))

(defn implicit-refer-ns
  "The namespace an unqualified, otherwise unresolved name is taken to come
  from in the Relay namespace `ns`. Whether a var of that name exists there, or
  in another referred namespace, is known once the cache is read."
  [ns name-sym]
  (when (:relay? ns)
    (some (fn [implicit-ns]
            (when-let [refer-all (get (:refer-alls ns) implicit-ns)]
              (when (and (:implicit refer-all)
                         (not (contains? (:excluded refer-all) name-sym)))
                implicit-ns)))
          implicit-namespaces)))

;;;; names Relay shares with Clojure

(def ^:private untyped-vars
  "Vars that take other arguments in Relay than their Clojure namesakes: throw
  takes any value."
  '#{throw})

(defn clojure-name?
  "True when clojure.core has a var or special form named `name-sym`."
  [name-sym]
  (or (var-info/core-sym? :clj name-sym)
      (contains? var-info/special-forms name-sym)
      (special-symbol? name-sym)))

(defn clojure-namesake
  "The clojure.core name of the var `name-sym` of the namespace `ns-sym`, when
  that is one of the namespaces Relay refers implicitly and Clojure has a var
  of that name. Such a var is analyzed and linted as the Clojure one: it has
  the same syntax and, unless listed otherwise, takes the same arguments."
  [ns-sym name-sym]
  (when (and (or (= core-ns ns-sym) (= builtins-ns ns-sym))
             (symbol? name-sym)
             (clojure-name? name-sym))
    name-sym))

(def ^:private special-forms-as
  "Relay's own special forms, by the Clojure form each is analyzed as."
  '{with-read-lock let
    with-write-lock let
    set-local! set!
    future* future})

(defn lint-as
  "The clojure.core var that a call of the var `name-sym` of `ns-sym` is
  analyzed as, as [ns name]: its namesake, or the form a special form of
  Relay's own has the syntax of."
  [ns-sym name-sym]
  (when-let [n (or (clojure-namesake ns-sym name-sym)
                   (when (= core-ns ns-sym)
                     (get special-forms-as name-sym)))]
    ['clojure.core n]))

(defn deferred-form
  "The special form that the var `name-sym` of `ns-sym` is, when it is one that
  keeps its body for later: after!, which evaluates it once the namespace of
  its libspec is loaded, or on-reload!, which evaluates it when its file is
  reloaded."
  [ns-sym name-sym]
  (when (= core-ns ns-sym)
    (get '{after! after!, on-reload! on-reload!} name-sym)))

(defn deferred-usage?
  "True when `usage` is in the body of a form that is evaluated later than the
  file it is in: it can name a var the file defines further down."
  [usage]
  (boolean (some (fn [[ns-sym name-sym]]
                   (deferred-form ns-sym name-sym))
                 (:callstack usage))))

(def in-place-vars
  "The vars that change a transient collection in place, so that their value
  is one the caller already has."
  '#{conj! assoc! dissoc! disj! pop!})

(defn as-clojure-var
  "The var `called-fn` for the linters that know Clojure's vars by name: under
  the namespace clojure.core when it is the namesake of one of them."
  [called-fn]
  (let [name-sym (:name called-fn)]
    (if (and (clojure-namesake (:ns called-fn) name-sym)
             (not (contains? untyped-vars name-sym)))
      (assoc called-fn :ns 'clojure.core)
      called-fn)))

;;;; resolving what a namespace refers implicitly

(defn implicit-namespaces-known?
  "True when the namespaces Relay refers implicitly were linted, in this run or
  in one that cached them. Only then is a name they lack known not to resolve."
  [idacs]
  (every? #(get-in idacs [:clj :defs %]) implicit-namespaces))

(defn- defined-before?
  "True unless `var-info` is defined in the file of `usage` and after it. The
  form that defines a var counts as a usage after it: the declaration of a
  special form calls the special form it declares."
  [usage var-info]
  (let [var-row (:row var-info)
        row (:row usage)]
    (or (not= (:top-ns usage) (:top-ns var-info))
        (not (and var-row row))
        (> row var-row)
        (and (= row var-row)
             (>= (:col usage) (:col var-info))))))

(defn- resolve-own-var
  "The var named `name-sym` of the namespace `usage` is made in, when the
  analysis did not see it there: another file of the namespace defines it, the
  call of a macro does, or the file defines it further down. In that last case
  the usage has to be one that can come first: in a form that is evaluated
  later, or of the var itself, as #'name is."
  [idacs usage name-sym]
  (when-let [var-info (get-in idacs [:clj :defs (:ns usage) name-sym])]
    (when (or (defined-before? usage var-info)
              (deferred-usage? usage)
              (:private-access? usage)
              (:in-comment usage))
      var-info)))

(defn- resolve-referred-var
  "The var named `name-sym` among those the namespace of `usage` refers without
  naming them: the public vars of the namespaces every Relay namespace refers,
  then those of the ones it uses, minus what it excludes."
  [idacs usage name-sym refer-alls]
  (some (fn [referred-ns]
          (when-not (contains? (:excluded (get refer-alls referred-ns)) name-sym)
            (when-let [var-info (get-in idacs [:clj :defs referred-ns name-sym])]
              (when (or (not (:private var-info))
                        (= referred-ns (:ns usage)))
                var-info))))
        (concat (filter #(contains? refer-alls %) implicit-namespaces)
                (remove (set implicit-namespaces) (keys refer-alls)))))

;;;; global vars

(defn global-name
  "The global name that the metadata map `var-meta` gives the var `var-name`:
  its own for `:global true`, the symbol of `:global name`, else nil."
  [var-meta var-name]
  (let [tag (:global var-meta)]
    (cond (true? tag) var-name
          (simple-symbol? tag) tag)))

(defn without-global-name
  "The metadata node `node` without the entry that holds a global's name, which
  is a symbol that nothing evaluates."
  [node]
  (if (identical? :map (:tag node))
    (assoc node :children
           (into []
                 (comp (partition-all 2)
                       (remove #(identical? :global (:k (first %))))
                       cat)
                 (:children node)))
    node))

(defn global-usage?
  "True when `usage` is of a name of the pseudo-namespace, `global/name`."
  [usage]
  (= global-ns (:resolved-ns usage)))

(defn- index-entry
  "What the index of the cached namespaces keeps of the namespace `ns-data`."
  [ns-data]
  {:dependencies (reduce into #{} (vals (:relay-dependencies ns-data)))
   :globals (reduce-kv (fn [globals var-name var-info]
                         (if-let [global (and (symbol? var-name)
                                              (map? var-info)
                                              (:global var-info))]
                           (assoc globals global var-name)
                           globals))
                       {}
                       ns-data)})

(defn index-namespaces
  "The index `index` with what this run found of the Relay namespaces of `defs`,
  those that `cached?`, a function of a filename, says are kept in the cache.

  The index maps each namespace to its direct dependencies and its globals, as
  a map from global name to var name. A lint needs them for every namespace of
  a tree, which can be most of the library, and the vars of one or two of those:
  this is the part of the namespaces that the cache keeps apart, so that a lint
  does not read them all."
  [index defs cached?]
  (reduce-kv (fn [index ns-sym ns-data]
               (if (and (:relay? ns-data)
                        (not (:source ns-data))
                        (cached? (:filename ns-data)))
                 (assoc index ns-sym (index-entry ns-data))
                 index))
             (or index {})
             defs))

(defn global-owners
  "A map from each global name of `index` to the vars that register it, sorted,
  each as a pair of its namespace and its name."
  [index]
  (let [owners (reduce-kv (fn [owners ns-sym {:keys [globals]}]
                            (reduce-kv (fn [owners global var-name]
                                         (update owners global (fnil conj []) [ns-sym var-name]))
                                       owners
                                       globals))
                          {}
                          index)]
    (reduce-kv (fn [owners global vars]
                 (assoc owners global (vec (sort vars))))
               owners
               owners)))

(defn scan-global-vars
  "The globals that the Relay source `text` defines in its namespace, as a map
  from global name to [namespace var], or nil. This reads the text, in place of
  analyzing it, so that what a call of a global is analyzed as is known before
  any file of a run is analyzed: only the name of the var matters, for its hooks
  and its :lint-as, and a tag in a comment names nothing that is called."
  [text]
  (when-let [tags (seq (re-seq #"\^(?::global|\{[^{}]*:global\s+(true|[^\s{}]+)[^{}]*\})\s+([^\s()\[\]{}]+)"
                               text))]
    (when-let [[_ ns-name] (re-find #"\(ns\s+([^\s()]+)" text)]
      (into {}
            (map (fn [[_ tag var-name]]
                   [(symbol (if (or (nil? tag) (= "true" tag)) var-name tag))
                    [(symbol ns-name) (symbol var-name)]]))
            tags))))

(defn global-vars
  "The var of each global of `index`, as a map from global name to
  [namespace var]."
  [index]
  (reduce-kv (fn [vars global owners]
               (assoc vars global (first owners)))
             {}
             (global-owners index)))

(defn global-var
  "The var, as [namespace name], that a call of the name `full-name`, resolved
  as `resolved-ns` and `unresolved?` say, is analyzed as when that is a global:
  its hooks and its :lint-as apply. Whether the namespace of the call sees it
  is decided once the cache is read."
  [ctx full-name resolved-ns unresolved?]
  (when (and (:relay? ctx)
             (or unresolved? (= global-ns resolved-ns)))
    (some-> (:relay-global-vars ctx)
            deref
            (get (symbol (name full-name))))))

(defn- user-namespace?
  "True for `user` and, with `config` saying so, for any namespace: a run for the
  user's configuration has that in its configuration."
  [ns-sym config]
  (or (= user-ns ns-sym)
      (true? (get-in config [:relay :user-configuration]))))

(defn- walk-tree
  "The namespaces that `roots` reach through the dependencies of `index`, and
  whether the index has all of them."
  [index roots]
  (loop [seen (set roots)
         todo (vec roots)
         complete? true]
    (if-let [ns-sym (peek todo)]
      (if-let [entry (get index ns-sym)]
        (let [new (remove seen (:dependencies entry))]
          (recur (into seen new) (into (pop todo) new) complete?))
        (recur seen (pop todo) false))
      {:namespaces seen :complete? complete?})))

(defn- namespace-tree
  "What the namespace `ns-sym` sees of the cached namespaces, as {:namespaces
  ... :complete? ...}: its tree and, when that holds a namespace the cache does
  not have yet, that it is not complete. A user namespace depends on the library
  and on every package, which is to say on all that is cached."
  [{:keys [relay-index relay-trees]} ns-sym user?]
  (let [key [ns-sym user?]]
    (or (get @relay-trees key)
        (let [tree (if user?
                     (assoc (walk-tree relay-index [ns-sym library-ns])
                            :namespaces (set (keys relay-index)))
                     (walk-tree relay-index [ns-sym]))]
          (swap! relay-trees assoc key tree)
          tree))))

(defn- visible-owner
  "The var that registers the global `name-sym` among those that `tree` holds."
  [idacs tree name-sym]
  (some #(when (contains? (:namespaces tree) (first %)) %)
        (get (:relay-owners idacs) name-sym)))

(defn resolve-global-var
  "The var that the global `name-sym` is for `usage`. Returns :unknown when no
  global of that name is known to be visible but the tree of the namespace of
  `usage` is not known yet, or the var's namespace is not cached."
  [idacs usage name-sym]
  (let [ns-sym (:ns usage)
        tree (namespace-tree idacs ns-sym (user-namespace? ns-sym (:config usage)))
        [owner-ns var-name] (visible-owner idacs tree name-sym)]
    (cond
      ;; a namespace sees its own globals as it sees its own vars: once they are defined
      (= ns-sym owner-ns) (resolve-own-var idacs usage var-name)
      owner-ns (or (some-> (get-in idacs [:clj :defs owner-ns var-name])
                           (assoc :relay-global true))
                   :unknown)
      (not (:complete? tree)) :unknown)))

(defn invisible-global-hint
  "What to add to the message about `usage` of `name-sym`, which resolves to
  nothing, when a global of that name exists that its namespace does not see."
  [idacs usage name-sym]
  (when-let [[owner-ns var-name] (first (get (:relay-owners idacs) name-sym))]
    (when-not (= owner-ns (:ns usage))
      (format ". It is the global #'%s/%s, registered by %s, which %s does not depend on: add it to the ns form's :require"
              owner-ns var-name owner-ns (:ns usage)))))

(defn newly-reached-dependencies
  "The direct dependencies of `ns-sym` through which it sees a global of
  `owner-ns`, the first time that a run asks: such a require is in use."
  [{:keys [relay-index relay-reached] :as idacs} ns-sym owner-ns]
  (let [reached [ns-sym owner-ns]]
    (when-not (contains? @relay-reached reached)
      (swap! relay-reached conj reached)
      (filter #(contains? (:namespaces (namespace-tree idacs % false)) owner-ns)
              (get-in relay-index [ns-sym :dependencies])))))

(defn var-definitions
  "What the analysis found the Relay namespace `ns` to define."
  [ns]
  (for [[var-name var-info] (:vars ns)
        :when (not (or (:in-comment var-info) (:temp var-info)))]
    {:ns (:name ns)
     :name var-name
     :global (:global var-info)
     :filename (:defined-in var-info)
     :row (or (:name-row var-info) (:row var-info))
     :col (or (:name-col var-info) (:col var-info))
     :end-row (or (:name-end-row var-info) (:end-row var-info))
     :end-col (or (:name-end-col var-info) (:end-col var-info))}))

(defn other-owners
  "The vars other than the one `definition` defines that register the global
  name it gives its var: those of the index, which holds one per namespace, and
  those of `definitions`, what this run found."
  [idacs definitions {:keys [ns name global]}]
  (->> (filter #(= global (:global %)) definitions)
       (map (juxt :ns :name))
       (concat (get (:relay-owners idacs) global))
       (remove #{[ns name]})
       distinct
       sort))

(defn shadowed-global
  "The var of the visible global that `definition`, a var of another name in
  the eyes of the registry, hides in its namespace."
  [idacs {:keys [ns name] :as _definition} config]
  (let [tree (namespace-tree idacs ns (user-namespace? ns config))
        owner (visible-owner idacs tree name)]
    (when (and owner (not= [ns name] owner))
      owner)))

(def ^:private runtime-vars
  "The vars of relay.core that the runtime defines and no file declares."
  '#{*ns*})

(def ^:private analysis-names
  "Names the analysis writes into the code it analyzes: `#(...)` is a `fn*`."
  '#{fn*})

(defn unqualified-usage?
  "True when `usage`, made in a Relay namespace, is of an unqualified name the
  analysis could not find in that namespace: one it took for a clojure.core
  var, or one it left unresolved. Relay finds such a name in the namespaces
  that the namespace refers, and nowhere else."
  [usage]
  (or (:unresolved? usage)
      (and (:resolved-core? usage)
           (:simple? usage))))

(defn used-names
  "The names that the Relay namespaces `namespaces` use in a way a global could
  stand for, and that the calls `candidates` make: the vars of those of them
  that are globals have to be read from the cache."
  [namespaces candidates]
  (-> #{}
      (into (comp (filter :relay?)
                  (mapcat :used-vars)
                  (filter #(or (unqualified-usage? %) (global-usage? %)))
                  (map :name))
            namespaces)
      (into (map :name) candidates)))

(defn resolve-unqualified-var
  "The var that `usage` of the unqualified `name-sym` refers to: one of its own
  namespace, else one of a namespace it refers, `refer-alls`. Returns :unknown
  when no namespace has it and it is unknown whether one should."
  [idacs usage name-sym refer-alls]
  (or (resolve-own-var idacs usage name-sym)
      (resolve-referred-var idacs usage name-sym refer-alls)
      (when (contains? runtime-vars name-sym)
        {:ns core-ns :name name-sym})
      (resolve-global-var idacs usage name-sym)
      (when (or (contains? analysis-names name-sym)
                (:clj-kondo.impl/generated (:expr usage))
                (not (implicit-namespaces-known? idacs)))
        :unknown)))

;;;; namespaces defined in several files

(defn forget-declared-vars
  "The namespace map `ns` once an `ns` form declares it in a second file, as
  when two copies of a library are read: without the vars of the first file,
  which the second defines again, and with those of the files that continue
  the namespace with `in-ns`. Such a file is one whose first namespace is
  another one, and it can be read before the files that declare the namespace
  or after them."
  [ns]
  (let [ns-name (:name ns)
        continued (into {}
                        (remove (fn [[_ var-info]] (= ns-name (:top-ns var-info))))
                        (:vars ns))]
    (-> ns
        (assoc :vars (not-empty continued))
        (update :var-counts select-keys (keys continued)))))

(defn merge-file-contributions
  "A Relay namespace can be defined in several files: the generated declarations
  of the builtins continue namespaces that Lisp files define. Returns
  `ns-data`, what this run found in such a namespace, with the vars and the
  dependencies the cache has of it from files this run did not analyze.
  `top-namespaces` names the files of this run, each by the first namespace it
  declares."
  [cached ns-data top-namespaces]
  (let [dependencies (into (or (:relay-dependencies ns-data) {})
                           (remove #(contains? top-namespaces (key %)))
                           (:relay-dependencies cached))]
    (reduce-kv (fn [acc k v]
                 (if (and (symbol? k)
                          (map? v)
                          (not (contains? acc k))
                          (not (contains? top-namespaces (:top-ns v))))
                   (assoc acc k v)
                   acc))
               (cond-> ns-data
                 (seq dependencies) (assoc :relay-dependencies dependencies))
               cached)))

(defn merge-cached-contributions
  "Adds to each Relay namespace of this run the vars the cache has of it from
  other files. `read-cached` reads a namespace of the cache."
  [idacs read-cached]
  (let [top-namespaces (or (:top-namespaces idacs) #{})]
    (update-in idacs [:clj :defs]
               (fn [defs]
                 (reduce-kv (fn [defs ns-sym ns-data]
                              (if (and (:relay? ns-data) (not (:source ns-data)))
                                (assoc defs ns-sym
                                       (merge-file-contributions (read-cached ns-sym)
                                                                 ns-data
                                                                 top-namespaces))
                                defs))
                            defs
                            defs)))))

;;;; macros that define vars

(defn normalize-definition-symbol
  "The :definition-symbol metadata of a macro as {:path [...] :type ...}: the
  path, through the arguments of a call of the macro, of the symbol that names
  the var the call defines, and the kind of var that is. The metadata is true
  for the first argument, a number for another one, a vector for a path into
  an argument, a keyword or string for the first argument and that type, or a
  map of both."
  [value]
  (let [definition (cond
                     (true? value) {:path [0] :type :variable}
                     (number? value) {:path [value] :type :variable}
                     (keyword? value) {:path [0] :type value}
                     (string? value) {:path [0] :type (keyword value)}
                     (vector? value) {:path value :type :variable}
                     (map? value) {:path (or (:path value) [0])
                                   :type (let [t (or (:type value) :variable)]
                                           (if (string? t) (keyword t) t))}
                     :else nil)]
    (when (and (vector? (:path definition))
               (every? nat-int? (:path definition)))
      definition)))

(defn- node-metadata
  "The metadata written on `node`, as a map."
  [node sexpr]
  (into {}
        (map (fn [meta-node]
               (if-let [k (:k meta-node)]
                 {k true}
                 (when (identical? :map (:tag meta-node))
                   (try (sexpr meta-node)
                        (catch Exception _ nil))))))
        (:meta node)))

(defn- symbol-entry [node sexpr]
  (let [value (:value node)]
    (when (simple-symbol? value)
      (let [metadata (node-metadata node sexpr)]
        (assoc (select-keys (meta node) [:row :col :end-row :end-col])
               :name value
               :private (boolean (:private metadata))
               :global (global-name metadata value))))))

(defn definition-candidate
  "What is kept of a call at the top level of a Relay file until the var it
  calls is known: should that be a macro with :definition-symbol metadata, the
  call defines a var. Of the arguments, the symbols at the paths such metadata
  uses are kept: an argument, or an element of an argument that is a vector
  or a list."
  [{:keys [ns top-ns filename]} expr call-info sexpr]
  (let [skeleton (mapv (fn [node]
                         (or (symbol-entry node sexpr)
                             (when (contains? #{:vector :list} (:tag node))
                               (mapv #(symbol-entry % sexpr) (:children node)))))
                       (take 3 (rest (:children expr))))]
    (when (some some? skeleton)
      (merge (select-keys (meta expr) [:row :col :end-row :end-col])
             call-info
             {:ns (:name ns)
              :top-ns top-ns
              :filename filename
              :skeleton skeleton}))))

(defn- candidate-callee
  [idacs {:keys [ns resolved-ns name unresolved?]}]
  (or (get-in idacs [:clj :defs resolved-ns name])
      (when unresolved?
        (or (get-in idacs [:clj :defs ns name])
            (some #(get-in idacs [:clj :defs % name]) implicit-namespaces)))
      (when (or unresolved? (= global-ns resolved-ns))
        (when-let [[owner-ns var-name] (first (get (:relay-owners idacs) name))]
          (get-in idacs [:clj :defs owner-ns var-name])))))

(defn register-definitions
  "Adds to the namespaces of this run the vars that calls of macros with
  :definition-symbol metadata define. The analysis took such a call for a
  function call, as the macro may be of a namespace it had not read."
  [idacs]
  (reduce (fn [idacs candidate]
            (let [{:keys [path type]} (:definition-symbol (candidate-callee idacs candidate))
                  entry (when path (get-in (:skeleton candidate) path))
                  var-path [:clj :defs (:ns candidate) (:name entry)]]
              (if (and (map? entry)
                       (:name entry)
                       (not (get-in idacs var-path)))
                (-> idacs
                    (assoc-in var-path
                              (cond-> {:ns (:ns candidate)
                                       :name (:name entry)
                                       :row (:row candidate)
                                       :col (:col candidate)
                                       :top-ns (:top-ns candidate)}
                                (:private entry) (assoc :private true)
                                (:global entry) (assoc :global (:global entry))
                                (identical? :macro type) (assoc :macro true)
                                (identical? :function type) (assoc :type :fn)))
                    (update :relay-macro-definitions (fnil conj [])
                            (assoc (select-keys entry [:name :global :row :col :end-row :end-col])
                                   :ns (:ns candidate)
                                   :filename (:filename candidate))))
                idacs)))
          idacs
          (:relay-definition-candidates idacs)))

;;;; strings

(defn unescape-string
  "The value of the text of a string literal as Relay reads it: the escapes
  for a newline, a tab, a return, a backslash and a double quote, and any other
  backslash as itself."
  [^String s]
  (let [n (.length s)
        sb (StringBuilder. n)]
    (loop [i 0]
      (if (< i n)
        (let [ch (.charAt s i)]
          (if (and (= \\ ch) (< (inc i) n))
            (let [escaped (.charAt s (inc i))]
              (case escaped
                \n (.append sb \newline)
                \t (.append sb \tab)
                \r (.append sb \return)
                \\ (.append sb \\)
                \" (.append sb \")
                (-> sb (.append ch) (.append escaped)))
              (recur (+ i 2)))
            (do (.append sb ch)
                (recur (inc i)))))
        (str sb)))))

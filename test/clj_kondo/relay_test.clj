(ns clj-kondo.relay-test
  "Relay Lisp, the language of `.rly` files: see clj-kondo.impl.relay. The files
  of corpus/relay stand in for the Relay library (`lib`), a program that uses
  it (`app`), a package (`pkg`) and a library with global vars and the
  programs that use them (`globals`)."
  (:require
   [babashka.fs :as fs]
   [clj-kondo.core :as clj-kondo]
   [clj-kondo.impl.config :as config]
   [clj-kondo.impl.relay :as relay]
   [clj-kondo.test-utils :refer [assert-submaps2 lint!]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def relay-auto-use
  "The auto-used namespaces as Relay's configuration names them: the runtime's, then Relay's."
  '[lisp.core lisp.builtins relay.core relay.builtins])

(def config
  {:relay {:auto-use relay-auto-use}
   :linters {:unresolved-symbol {:level :error}
             :unresolved-var {:level :warning}
             :unresolved-namespace {:level :warning}
             :unused-value {:level :warning}
             :unused-binding {:level :warning}
             :unused-private-var {:level :off}
             :private-call {:level :off}
             :namespace-name-mismatch {:level :error}}})

(def lib (io/file "corpus" "relay" "lib"))

(def app (io/file "corpus" "relay" "app" "app.rly"))

(def pkg (io/file "corpus" "relay" "pkg"))

(def app-findings
  '({:file "corpus/relay/app/app.rly" :row 12 :col 4 :level :error
     :message "Unresolved symbol: println"}
    {:file "corpus/relay/app/app.rly" :row 13 :col 4 :level :error
     :message "Unresolved symbol: frobnicate"}
    {:file "corpus/relay/app/app.rly" :row 14 :col 4 :level :error
     :message "Unresolved symbol: private-helper"}
    {:file "corpus/relay/app/app.rly" :row 21 :col 4 :level :warning
     :message "Unresolved namespace nowhere.ns. Are you missing a require?"}
    {:file "corpus/relay/app/app.rly" :row 33 :col 5 :level :error
     :message "catch requires an error type, a binding symbol and at least one body form"}
    {:file "corpus/relay/app/app.rly" :row 37 :col 3 :level :error
     :message "Unresolved symbol: later-state"}))

(deftest library-and-program-test
  (testing "a program resolves what it refers of the runtime's lisp.core and
  lisp.builtins and of relay.core and relay.builtins, the vars that macros define, qualified names of loaded namespaces, and what
  forms evaluated later name; the library itself lints clean"
    (assert-submaps2 app-findings (lint! [lib app] config))))

(deftest unknown-library-test
  (testing "nothing is said about an unqualified name while the auto-used namespaces
  are unknown"
    (assert-submaps2
     '({:row 19 :col 4 :message "Unresolved namespace relay.other-ns. Are you missing a require?"}
       {:row 21 :col 4 :message "Unresolved namespace nowhere.ns. Are you missing a require?"}
       {:row 33 :col 5 :message "catch requires an error type, a binding symbol and at least one body form"})
     (lint! app config))))

(deftest package-test
  (testing "the description of a package is data, and the namespace of a file
  under lib/ or of a test under test/ is named after its file"
    (assert-submaps2
     '({:file "corpus/relay/pkg/lib/pkg/misnamed.rly" :row 1 :col 5 :level :error
        :message "Namespace name does not match file name: relay.pkg.other-name"}
       {:file "corpus/relay/pkg/test/pkg/misnamed.rly" :row 1 :col 5 :level :error
        :message "Namespace name does not match file name: pkg.tests.other-name"})
     (lint! [lib pkg] config))))

(deftest package-file-names-test
  (testing "a package keeps its namespaces in lib/, those that the relay prefix
  was added to among them, and its tests in test/"
    (is (relay/file-names-namespace? "packages.pkg.lib.pkg.core" 'pkg.core))
    (is (relay/file-names-namespace? "packages.pkg.lib.pkg.extra" 'relay.pkg.extra))
    (is (relay/file-names-namespace? "packages.pkg.test.pkg.core" 'pkg.tests.core))
    (is (relay/file-names-namespace? "packages.pkg.test.pkg.core" 'relay.tests.pkg.core))
    (is (not (relay/file-names-namespace? "packages.pkg.lib.pkg.extra" 'relay.pkg.other)))
    (is (not (relay/file-names-namespace? "packages.pkg.doc.pkg.extra" 'relay.pkg.extra))))
  (testing "a package that keeps its Lisp in src/, which Relay reads for a
  transition, has its namespaces there"
    (is (relay/file-names-namespace? "packages.pkg.src.pkg.extra" 'relay.pkg.extra))
    (is (relay/file-names-namespace? "packages.pkg.src.pkg.core" 'pkg.core))))

(deftest clojure-file-test
  (testing "a Clojure file is linted as before"
    (assert-submaps2
     '({:row 1 :col 30 :message "Unresolved symbol: exception"}
       {:row 1 :col 47 :message "Unresolved symbol: message"})
     (lint! "(ns app) (try (inc 1) (catch exception error (message error)))"
            config))))

(defn- run-findings [cache-dir & paths]
  (->> (clj-kondo/run! {:lint (map str paths)
                        :cache-dir (str cache-dir)
                        :config [config]})
       :findings
       (map #(select-keys % [:row :col :message]))
       (sort-by (juxt :row :col))))

(deftest cache-test
  (fs/with-temp-dir [cache-dir {}]
    (clj-kondo/run! {:lint [(str lib)]
                     :cache-dir (str cache-dir)
                     :dependencies true
                     :config [config]})
    (let [expected (map #(select-keys % [:row :col :message]) app-findings)]
      (testing "a program is linted against a cached library, including the vars
  its macros define"
        (is (= expected (run-findings cache-dir app))))
      (testing "linting one file of a namespace keeps what the cache has of it
  from its other files"
        (is (empty? (run-findings cache-dir (io/file lib "relay" "string.rly"))))
        (is (empty? (run-findings cache-dir (io/file lib "relay" "builtins-extra.rly"))))
        (is (empty? (run-findings cache-dir (io/file lib "relay" "core.rly"))))
        (is (= expected (run-findings cache-dir app)))))))

(deftest library-copies-test
  (testing "a namespace that two files declare, as when two copies of a library
  are read, keeps what the files that continue it with in-ns define, whichever
  are read first"
    (fs/with-temp-dir [dir {}]
      (let [copies (mapv (fn [copy]
                           (let [target (fs/file dir copy)]
                             (fs/copy-tree lib target)
                             target))
                         ["one" "two"])
            files (fn [name] (mapv #(str (io/file % "relay" name)) copies))
            runtime (for [copy copies
                          name ["core.rly" "builtins.rly" "builtins-extra.rly"]]
                      (str (io/file copy "lisp" name)))
            expected (map #(select-keys % [:row :col :message]) app-findings)]
        (doseq [order [["builtins-extra.rly" "core.rly" "string.rly"]
                       ["core.rly" "builtins-extra.rly" "string.rly"]
                       ["core.rly" "string.rly" "builtins-extra.rly"]]]
          (fs/with-temp-dir [cache-dir {}]
            (clj-kondo/run! {:lint (concat runtime
                                           (mapcat files order)
                                           (files "builtins.rly")
                                           (files "other-ns.rly"))
                             :cache-dir (str cache-dir)
                             :dependencies true
                             :config [config]})
            (is (= expected (run-findings cache-dir app)) (str order))))))))

(deftest private-var-of-a-continued-namespace-test
  (testing "an unused private var is reported in the file that defines it, where an
  ignore applies, whichever file that continues its namespace with in-ns is read last"
    (fs/with-temp-dir [dir {}]
      (let [config (-> config
                       (assoc-in [:linters :unused-private-var :level] :warning)
                       (assoc-in [:linters :redundant-ignore :level] :warning))
            core (fs/file dir "relay" "core.rly")
            extra (fs/file dir "relay" "builtins-extra.rly")]
        (fs/create-dirs (fs/parent core))
        (spit core (str "(ns relay.core)\n\n"
                        "#_{:clj-kondo/ignore [:unused-private-var]}\n"
                        "(defn- kept-for-the-runtime [] nil)\n\n"
                        "(defn- unused [] nil)\n"))
        (spit extra "(ns relay.builtins-extra)\n\n(in-ns 'relay.core)\n(def ^:special-form def nil)\n")
        (doseq [order [[core extra] [extra core]]]
          (assert-submaps2
           [{:file (str core) :row 6 :col 8 :message "Unused private var relay.core/unused"}]
           (lint! order config)))))))

;;;; the auto-used namespaces

(deftest configured-var-of-a-later-auto-used-namespace-test
  (testing "a :lint-as or a hook that names a var of an auto-used namespace after the
  first applies to its unqualified name, which resolves to the first one until the cache
  is read"
    (let [file (io/file "corpus" "relay" "app" "lint-as.rly")]
      (assert-submaps2
       '({:file "corpus/relay/app/lint-as.rly" :row 5 :col 13 :message "Unresolved symbol: x"})
       (lint! [lib file] config))
      (assert-submaps2
       []
       (lint! [lib file] (assoc config :lint-as '{relay.core/each-of clojure.core/for}))))))

(def host (io/file "corpus" "relay" "host"))

(def host-core (io/file host "host" "core.rly"))

(def host-app (io/file host "host-app.rly"))

(def host-config
  (assoc config :relay {:auto-use (conj relay-auto-use 'host.core)}))

(def host-findings
  '({:file "corpus/relay/host/host-app.rly" :row 6 :col 3 :level :warning
     :message "Unused value"}
    {:file "corpus/relay/host/host-excludes.rly" :row 6 :col 4 :level :error
     :message "Unresolved symbol: host-fn"}))

(deftest auto-used-namespaces-test
  (testing "every namespace refers the vars of the namespaces that the configuration
  auto-uses, a namesake of clojure.core is analyzed as the Clojure var, a qualified name
  needs no require, and :refer-clojure excludes a name"
    (assert-submaps2 host-findings (lint! [lib host] host-config)))
  (testing "without the key, they are the runtime's lisp.core and lisp.builtins"
    (is (= '[lisp.core lisp.builtins] (relay/auto-used-namespaces (dissoc config :relay))))
    (is (= relay-auto-use (relay/auto-used-namespaces config)))
    (assert-submaps2
     '({:file "corpus/relay/host/host-app.rly" :row 5 :col 4 :message "Unresolved symbol: host-fn"}
       {:file "corpus/relay/host/host-app.rly" :row 6 :col 4 :message "Unresolved symbol: assoc"}
       {:file "corpus/relay/host/host-excludes.rly" :row 6 :col 4 :message "Unresolved symbol: host-fn"})
     (lint! [lib host] config)))
  (testing "nothing is said about an unqualified name while an auto-used namespace is
  unknown, and its qualified names are not of an unresolved namespace"
    (assert-submaps2 [] (lint! [lib host-app] host-config))
    (assert-submaps2
     '({:row 5 :col 4 :message "Unresolved symbol: host-fn"}
       {:row 6 :col 4 :message "Unresolved symbol: assoc"}
       {:row 7 :col 4 :message "Unresolved namespace host.core. Are you missing a require?"})
     (lint! [lib host-app] config)))
  (testing "a later configuration's list replaces an earlier one's"
    (is (= '[relay.core]
           (relay/auto-used-namespaces
            (config/merge-config! host-config {:relay {:auto-use '[relay.core]}}))))
    (is (= ["Unresolved symbol: host-fn" "Unresolved symbol: assoc" "Unresolved symbol: host-fn"]
           (->> (clj-kondo/run! {:lint (map str [lib host])
                                 :cache false
                                 :config [host-config {:relay {:auto-use relay-auto-use}}]})
                :findings
                (map :message))))))

(deftest cached-auto-used-namespace-test
  (testing "a namespace is linted against a cached auto-used namespace, in a run of its
  own and in a parallel one"
    (fs/with-temp-dir [cache-dir {}]
      (clj-kondo/run! {:lint (map str [lib host-core])
                       :cache-dir (str cache-dir)
                       :dependencies true
                       :config [host-config]})
      (doseq [parallel [false true]]
        (is (= (map (juxt :row :message) host-findings)
               (->> (clj-kondo/run! {:lint (map str [host-app (io/file host "host-excludes.rly")])
                                     :cache-dir (str cache-dir)
                                     :parallel parallel
                                     :config [host-config]})
                    :findings
                    (sort-by (juxt :filename :row :col))
                    (map (juxt :row :message))))
            (str "parallel " parallel))))))

(def base (io/file "corpus" "relay" "base"))

(deftest special-forms-namespace-test
  (testing "the special forms are those of the first auto-used namespace, whichever it is:
  a deferred form can name what the file defines below, with-read-lock binds, and *ns* is
  the runtime's; relay.core, later in the list, is referred as the others are"
    (assert-submaps2
     []
     (lint! [base (io/file lib "relay" "builtins.rly") (io/file lib "relay" "other-ns.rly")]
            (assoc config :relay {:auto-use '[base.core relay.core relay.builtins]})))))

;;;; global vars

(def globals (io/file "corpus" "relay" "globals"))

(defn- globals-file [& path]
  (apply io/file globals path))

(def globals-lib [lib (globals-file "src" "lib")])

(def globals-config
  (-> config
      (assoc-in [:linters :invalid-arity :level] :error)
      (assoc-in [:linters :unused-namespace :level] :warning)
      (assoc-in [:linters :unused-referred-var :level] :off)
      (assoc-in [:linters :namespace-name-mismatch :level] :off)
      (assoc-in [:linters :unresolved-var :report-duplicates] true)))

(defn- hint
  "The end of the message about a global that the namespace `ns` does not see."
  [var-name owner ns]
  (format ". It is the global #'%s/%s, registered by %s, which %s does not depend on: add it to the ns form's :require"
          owner var-name owner ns))

(defn- findings-of
  "What linting `paths` with an existing cache says, by file name, row and message."
  [cache-dir config & paths]
  (->> (clj-kondo/run! {:lint (map str paths)
                        :cache-dir (str cache-dir)
                        :config [config]})
       :findings
       (map (juxt #(fs/file-name (:filename %)) :row :message))
       sort
       vec))

(defn- with-globals-cache
  "Calls `f` with a cache that the library of global vars was read into."
  [f]
  (fs/with-temp-dir [cache-dir {}]
    (clj-kondo/run! {:lint (map str globals-lib)
                     :cache-dir (str cache-dir)
                     :dependencies true
                     :config [globals-config]})
    (f cache-dir)))

(def uses-findings
  [["uses.rly" 30 "lib.base/base-fn is called with 0 args but expects 1"]
   ["uses.rly" 31 "lib.base/base-fn is called with 0 args but expects 1"]
   ["uses.rly" 41 (str "Unresolved symbol: outside-fn" (hint "outside-fn" "lib.outside" "app.uses"))]
   ["uses.rly" 42 (str "Unresolved var: global/outside-fn" (hint "outside-fn" "lib.outside" "app.uses"))]
   ["uses.rly" 43 "Unresolved var: global/nowhere"]
   ["uses.rly" 44 "Unresolved symbol: nowhere"]
   ["uses.rly" 51 (str "Unresolved symbol: nested-fn" (hint "nested-fn" "lib.nested" "app.uses"))]
   ["uses.rly" 52 (str "Unresolved symbol: aliased-fn" (hint "aliased-fn" "lib.aliased" "app.uses"))]
   ["uses.rly" 58 (str "Unresolved symbol: used-fn" (hint "used-fn" "lib.used" "app.uses"))]])

(deftest global-resolution-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "a namespace names the globals of its tree, through a require of any depth
  or at the top level, unqualified and through global/; a var keeps its arity; the
  rest of the globals are out of sight, and a nested require and :as-alias do not count"
        (is (= uses-findings
               (findings-of cache-dir globals-config (globals-file "src" "app" "uses.rly"))))))))

(deftest global-analyzed-with-the-file-test
  (testing "the same goes for a run that reads the library along with the file"
    (fs/with-temp-dir [cache-dir {}]
      (is (= uses-findings
             (filterv #(= "uses.rly" (first %))
                      (apply findings-of cache-dir globals-config
                             (conj globals-lib (globals-file "src" "app" "uses.rly")))))))))

(deftest own-globals-test
  (testing "a namespace sees a global of its own under any name, once it is defined"
    (fs/with-temp-dir [cache-dir {}]
      (is (= [["base.rly" 38 "Unresolved symbol: later-global"]]
             (apply findings-of cache-dir globals-config globals-lib))))))

(deftest shadowed-names-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "an own definition, a referral and a local come first, without a word"
        (is (= [] (findings-of cache-dir globals-config
                               (globals-file "src" "app" "shadows.rly")))))
      (testing "and :relay/shadowed-global reports the definition, when it is on"
        (is (= [["shadows.rly" 6 "base-value shadows the global #'lib.base/base-value"]]
               (findings-of cache-dir
                            (assoc-in globals-config [:linters :relay/shadowed-global :level] :warning)
                            (globals-file "src" "app" "shadows.rly"))))))))

(deftest required-for-a-global-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "a require that a global is reached through is in use"
        (is (= [["requires.rly" 3 "namespace lib.outside is required but never used"]]
               (findings-of cache-dir globals-config
                            (globals-file "src" "app" "requires.rly"))))))))

(def binding-hook
  "A hook that makes (name binding body...) a let."
  "(require '[clj-kondo.hooks-api :as api])
   (fn [{:keys [node]}]
     (let [[_ binding & body] (:children node)]
       {:node (api/list-node (list* (api/token-node 'let) binding body))}))")

(deftest analyzed-as-the-var-of-a-global-test
  (testing "a call of a global is analyzed as its var, by its own name and by another:
  its hooks and its :lint-as apply, in a later run and in the one that first reads
  the library"
    (doseq [config [(assoc globals-config
                           :lint-as {'lib.hooked/with-binding 'clojure.core/let
                                     'lib.hooked/renamed-binding 'clojure.core/let})
                    (assoc globals-config
                           :hooks {:__dangerously-allow-string-hooks__ true
                                   :analyze-call {'lib.hooked/with-binding binding-hook
                                                  'lib.hooked/renamed-binding binding-hook}})]
            :let [file (globals-file "src" "app" "hooks.rly")]]
      (with-globals-cache
        (fn [cache-dir]
          (is (= [] (findings-of cache-dir config file)))))
      (fs/with-temp-dir [cache-dir {}]
        (is (= [] (filterv #(= "hooks.rly" (first %))
                           (apply findings-of cache-dir config (conj globals-lib file)))))))))

(deftest tags-outside-of-code-test
  (let [texts (globals-file "src" "app" "texts.rly")
        hooks (globals-file "src" "app" "hooks.rly")
        config (assoc globals-config
                      :lint-as {'lib.hooked/with-binding 'clojure.core/let
                                'lib.hooked/renamed-binding 'clojure.core/let})]
    (testing "the look through a file's text for tags reads its code alone: a string, a
  regex, a comment and a character literal hold none"
      (is (= '{texts-value [app.texts texts-value]
               texts-after-semicolon [app.texts texts-after-semicolon]
               texts-renamed [app.texts texts-other-name]}
             (relay/scan-global-vars (slurp texts)))))
    (testing "so a file that shows the tag of another namespace's global in a string or a
  comment changes nothing of what a call of the global is analyzed as, whichever file the
  run reads first"
      (doseq [paths [(conj globals-lib hooks texts) (into [texts hooks] globals-lib)]]
        (fs/with-temp-dir [cache-dir {}]
          (is (= [] (filterv #(= "hooks.rly" (first %))
                             (apply findings-of cache-dir config paths)))
              (str (mapv fs/file-name paths))))))
    (testing "where two texts tag one name, the first var by namespace is the one,
  whatever their order"
      (let [a "(ns lib.a)\n(defmacro ^:global shared [] nil)\n"
            b "(ns lib.b)\n(defmacro ^:global shared [] nil)\n"]
        (is (= '{shared [lib.a shared]} (relay/scanned-global-vars [a b])))
        (is (= '{shared [lib.a shared]} (relay/scanned-global-vars [b a])))))))

(deftest duplicate-global-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "a global name that two vars give is reported at each definition in the run"
        (is (= [["duplicate.rly" 3 "Global middle-fn is also registered as #'lib.middle/middle-fn"]
                ["duplicate.rly" 6 "Global twice is also registered as #'app.duplicate/twice-again"]
                ["duplicate.rly" 9 "Global twice is also registered as #'app.duplicate/twice"]]
               (findings-of cache-dir globals-config
                            (globals-file "src" "app" "duplicate.rly"))))))))

;;;; aliases

(def alias-files
  {"target.rly" "(ns lib.target)\n\n(defn join [sep coll]\n  [sep coll])\n"
   "aliases.rly" (str "(ns lib.aliases\n  (:require [lib.target :as target]))\n\n"
                      "(defalias join target/join)\n"
                      "(defalias ^:global global-join target/join)\n"
                      "(defalias own join)\n\n"
                      "(join 1)\n"
                      "(own 1 2 3)\n")
   "app.rly" (str "(ns app.aliases\n  (:require [lib.aliases :as aliases]))\n\n"
                  "(aliases/join 1 2)\n"
                  "(aliases/join 1)\n"
                  "(global-join 1 2 3)\n"
                  "(aliases/own)\n")})

(deftest alias-test
  (testing "a name that defalias defines is linted as the var it stands for: called in its
  namespace, through that namespace, as a global and through another alias, in a run and
  from the cache"
    (fs/with-temp-dir [dir {}]
      (doseq [[name text] alias-files]
        (spit (fs/file dir name) text))
      (let [[target aliases app] (map #(fs/file dir %) ["target.rly" "aliases.rly" "app.rly"])
            expected [["aliases.rly" 8 "lib.target/join is called with 1 arg but expects 2"]
                      ["aliases.rly" 9 "lib.target/join is called with 3 args but expects 2"]
                      ["app.rly" 5 "lib.target/join is called with 1 arg but expects 2"]
                      ["app.rly" 6 "lib.target/join is called with 3 args but expects 2"]
                      ["app.rly" 7 "lib.target/join is called with 0 args but expects 2"]]]
        (is (= expected
               (->> (clj-kondo/run! {:lint (map str [lib target aliases app])
                                     :cache false
                                     :config [globals-config]})
                    :findings
                    (map (juxt #(fs/file-name (:filename %)) :row :message))
                    sort
                    vec)))
        (fs/with-temp-dir [cache-dir {}]
          (clj-kondo/run! {:lint (map str [lib target aliases])
                           :cache-dir (str cache-dir)
                           :dependencies true
                           :config [globals-config]})
          (is (= (filterv #(= "app.rly" (first %)) expected)
                 (findings-of cache-dir globals-config app))))))))

(deftest user-namespace-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "user sees every cached global"
        (is (= [["init.rly" 4 "Unresolved symbol: nowhere"]]
               (findings-of cache-dir globals-config (globals-file "config" "init.rly")))))
      (testing "a file of the user's configuration too, when the run says it is one"
        (is (= [["helpers.rly" 4 (str "Unresolved symbol: outside-fn"
                                       (hint "outside-fn" "lib.outside" "my.helpers"))]]
               (findings-of cache-dir globals-config (globals-file "config" "helpers.rly"))))
        (is (= [] (findings-of cache-dir
                               (assoc globals-config :relay {:user-configuration true})
                               (globals-file "config" "helpers.rly"))))))))

(deftest unknown-tree-test
  (testing "nothing is said about an unqualified name while a namespace of the tree is
  not cached yet"
    (fs/with-temp-dir [cache-dir {}]
      (clj-kondo/run! {:lint [(str lib)]
                       :cache-dir (str cache-dir)
                       :dependencies true
                       :config [globals-config]})
      (is (= [] (filter #(re-find #"Unresolved" (nth % 2))
                        (findings-of cache-dir globals-config
                                     (globals-file "src" "app" "requires.rly"))))))))

(deftest globals-of-a-continued-namespace-test
  (testing "a global that another file declares of a namespace is kept when either is
  linted again, and so is what the namespace depends on"
    (fs/with-temp-dir [dir {}]
      (let [base (fs/file dir "base.rly")
            extra (fs/file dir "extra.rly")
            app (fs/file dir "app.rly")]
        (spit base "(ns lib.continued\n  (:require [relay.string :as str]))\n\n(defn f [s] (str/trim s))\n")
        (spit extra "(ns lib.extra)\n\n(in-ns 'lib.continued)\n(declare ^:global builtin-global)\n")
        (spit app "(ns app.continued\n  (:require [lib.continued :as continued]))\n\n(continued/f 1)\n(builtin-global 1)\n")
        (fs/with-temp-dir [cache-dir {}]
          (clj-kondo/run! {:lint (map str [lib base extra])
                           :cache-dir (str cache-dir)
                           :dependencies true
                           :config [globals-config]})
          (is (= [] (findings-of cache-dir globals-config app)))
          (doseq [file [base extra base]]
            (findings-of cache-dir globals-config file)
            (is (= [] (findings-of cache-dir globals-config app))
                (str file))))))))

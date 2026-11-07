(ns clj-kondo.relay-test
  "Relay Lisp, the language of `.rly` files: see clj-kondo.impl.relay. The files
  of corpus/relay stand in for the Relay library (`lib`), a program that uses
  it (`app`), a package (`pkg`) and a library with global vars and the
  programs that use them (`globals`)."
  (:require
   [babashka.fs :as fs]
   [clj-kondo.core :as clj-kondo]
   [clj-kondo.test-utils :refer [assert-submaps2 lint!]]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(def config
  {:linters {:unresolved-symbol {:level :error}
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
  (testing "a program resolves what it refers of relay.core and relay.builtins,
  the vars that macros define, qualified names of loaded namespaces, and what
  forms evaluated later name; the library itself lints clean"
    (assert-submaps2 app-findings (lint! [lib app] config))))

(deftest unknown-library-test
  (testing "nothing is said about an unqualified name while relay.core and
  relay.builtins are unknown"
    (assert-submaps2
     '({:row 19 :col 4 :message "Unresolved namespace relay.other-ns. Are you missing a require?"}
       {:row 21 :col 4 :message "Unresolved namespace nowhere.ns. Are you missing a require?"}
       {:row 33 :col 5 :message "catch requires an error type, a binding symbol and at least one body form"})
     (lint! app config))))

(deftest package-test
  (testing "the description of a package is data, and the namespace of a test
  is named after its file under test/"
    (assert-submaps2
     '({:file "corpus/relay/pkg/test/pkg/misnamed.rly" :row 1 :col 5 :level :error
        :message "Namespace name does not match file name: pkg.tests.other-name"})
     (lint! [lib pkg] config))))

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
            expected (map #(select-keys % [:row :col :message]) app-findings)]
        (doseq [order [["builtins-extra.rly" "core.rly" "string.rly"]
                       ["core.rly" "builtins-extra.rly" "string.rly"]
                       ["core.rly" "string.rly" "builtins-extra.rly"]]]
          (fs/with-temp-dir [cache-dir {}]
            (clj-kondo/run! {:lint (concat (mapcat files order)
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

(deftest duplicate-global-test
  (with-globals-cache
    (fn [cache-dir]
      (testing "a global name that two vars give is reported at each definition in the run"
        (is (= [["duplicate.rly" 3 "Global middle-fn is also registered as #'lib.middle/middle-fn"]
                ["duplicate.rly" 6 "Global twice is also registered as #'app.duplicate/twice-again"]
                ["duplicate.rly" 9 "Global twice is also registered as #'app.duplicate/twice"]]
               (findings-of cache-dir globals-config
                            (globals-file "src" "app" "duplicate.rly"))))))))

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

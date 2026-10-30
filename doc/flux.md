# Flux Lisp

flux-kondo lints Flux Lisp, the language of the [Flux](https://github.com/xjeffw/flux) editor,
in files named `*.flux`. Flux Lisp is based on Clojure, and such a file is analyzed as Clojure
except for what follows. `src/clj_kondo/impl/flux.clj` implements it, and
`test/clj_kondo/flux_test.clj` with the files of `corpus/flux` tests it. Files of other
languages are linted as clj-kondo lints them.

## What a Flux file is linted as

- **Implicit refers.** Every namespace refers the public vars of `flux.core` and
  `flux.builtins`, as if by `(:use flux.core flux.builtins)`, minus the names its
  `:refer-clojure` clause excludes. The two namespaces take the place of `clojure.core`: an
  unqualified name is a var of the namespace, of one it refers, or it is unresolved. A name
  that only Clojure has, such as `println`, is unresolved. Flux declares its special forms as
  vars of `flux.core`, so they are found the same way. A var the two namespaces have in common
  with `clojure.core` is analyzed as the Clojure one, and its arity is the one its Flux
  definition has.
- **Unknown library.** While `flux.core` or `flux.builtins` has not been linted, in this run
  or in one that cached it, nothing is reported about an unqualified name.
- **Global vars.** See below.
- **Qualified names.** A qualified name resolves in any namespace that has been linted, whether
  the file requires it or not, as Flux resolves it in any loaded namespace.
- **Namespaces in several files.** A namespace can be continued in another file with `in-ns`,
  as the generated declarations of the builtins continue the namespaces that Lisp files define.
  The cache keeps what each file contributes: linting one file replaces its part alone. A file
  that continues a namespace is told from one that declares it by the first namespace it names,
  so it begins with an `ns` form of its own. Files are read in any order, and a run can read
  two files that declare one namespace, as two copies of a library do: the second replaces
  what the first defined, and neither replaces what a continuing file did.
- **Macros that define vars.** A macro whose metadata has `:definition-symbol` defines, where
  it is called at the top level of a file, the var its argument at that path names:
  `{:path [0] :type :variable}` for the first argument, `{:path [0 1] :type :function}` for the
  second element of the first. The var is known once the macro is, so a call needs no
  `:lint-as`.
- **Special forms.** `(catch type name body...)` leaves `type` unevaluated. `with-read-lock` and
  `with-write-lock` bind as `let`, `set-local!` is `set!`, `future*` is `future`.
  `(after! libspec body...)` requires as its libspec says, and its body, like that of
  `on-reload!`, is evaluated later: it can name what the file defines further down, and
  redefine a var. So can `#'name`.
- **Smaller differences.** `^:private` does not keep another namespace from a var. A string
  keeps an escape the reader does not know. A vector takes an index and a default. `conj!` and
  its like change their collection in place, so their value can go unused. A file is named
  after its namespace as written, and a package keeps `P.tests.X` in `test/P/X.flux`. A
  `package.flux` that holds one map is data.

## Global vars

A var with `:global` metadata is a global: `(defn ^:global add-hook! ...)` under its own name,
`^{:global all-buffers}` under another one, and `false` not at all. Flux's manual has the page
"Global Vars", and Flux's `plans/global-vars.org` the design. What flux-kondo does with them:

- **Definitions.** The metadata is read from a name of the `def` family (the forms that are
  analyzed as `def`, `defn` or `defmacro`, a `:lint-as` to one of them included), of `declare`,
  and of the macros with `:definition-symbol`, which covers `defalias`. The symbol of
  `^{:global name}` is not evaluated, so it is not linted as a usage. A var that registers
  itself some other way, with `alter-meta!` at runtime, is not known.
- **Dependencies.** A namespace's dependencies are what its `ns` form's `:require` and `:use`
  name and what `require` and `use` forms directly at the top level of its file name. A libspec
  with only `:as-alias` does not count, nor does a require in another form, a `do` included, nor
  the libspec of `after!`: these are the forms that Flux's loader reads. Its tree is itself and
  the trees of its dependencies.
- **Resolution.** An unqualified name that is nothing else in the namespace, including what the
  namespace refers, is looked up among the globals of its tree. So is `global/name`, and the
  namespace `global` is never unresolved. A global's var is linted as any var is, for its arity
  and the like, and a namespace sees its own globals once they are defined.
- **Out of sight.** A name that a global of a namespace outside of the tree has is unresolved,
  and the message says which namespace to require. While a namespace of the tree has not been
  linted, in this run or in one that cached it, nothing is reported about the name.
- **Calls.** A call of a global is analyzed as a call of its var: its hooks, `:lint-as` and
  `:config-in-call` apply, by the var's name. So that this needs no earlier run, a run first
  looks through the text of the Flux files it was given for such tags, and otherwise goes by the
  cache.
- **Usage.** A global counts as a use of each direct dependency that has its namespace in its
  tree, so `:unused-namespace` does not report a require that exists for its globals.
- **User namespaces.** `user`, which is where a file without an `ns` form is, sees every global
  that is linted or cached, as Flux's `user` depends on the library and on every package. So does
  every namespace of a run configured with `{:flux {:user-configuration true}}`, which the Flux
  language server gives the files of the user's configuration. Their tree is known once
  `flux.lib`'s is.

Two linters are about globals, and are configured as any other:

- `:flux/duplicate-global` (a warning): a definition that gives a global name that another var,
  of this run or of the cache, registers. Flux refuses to load the second.
- `:flux/shadowed-global` (off): a definition that hides a global its namespace sees. Nothing in
  Flux warns about this, since the definition simply comes first.

The cache keeps the dependencies and the global names with each namespace, and a digest of both
for all namespaces in one file, `flux/index.transit.json`, so that resolving a name does not
read every namespace of a tree. It is replaced as a whole, since a run reads it before it locks
the cache. A cache written before flux-kondo knew globals has no such file, and is known not
to hold a tree until it has been filled again.

## Configuration

Configuration is clj-kondo's, in `.clj-kondo/config.edn`. In addition, the directory that the
environment variable `FLUX_KONDO_LIBRARY_CONFIG_DIR` names is read as a configuration directory
before the project's, which can therefore change what it says. The Flux language server sets
it to the directory of the Flux library that holds the hooks for the library's macros.

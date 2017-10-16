---
layout: post
title: The Analyzer
---
To determine *what the user meant*, the analyzer tracks **environments**, produces **AST nodes**, and organizes extendable **analysis passes** which MAGIC leverages.

## Environments

The same form can have different meanings depending on *where* it occurs. Consider the form `(str a ", " b)` in the following examples:

```clj
;; Example 1
(defn join-two [a b]
  (str a ", " b))

;; Example 2
(let [a "Hello"
      b "World"]
  (str a ", " b))

;; Example 3
(def a "Hello")
(def b "World")
(str a ", " b)
```

In these three examples `a` and `b` refer to function parameters, local variables, and vars respectively. The same form can have considerably different meaning (and down the line generate different bytecode) depending on *where* in the source it appears. Another way to think about it is that s-expressions alone are insufficient to analyze. We need an additional data structure to represent where a form appears in the source. As a result `tools.analyzer`'s analysis functions generally take two arguments: the form to analyze and *the environment* that form was in.

The basic structure of an environment is revealed by an empty environment produced by `clojure.tools.analyzer/empty-env`

```clj
;; ns magic.analyzer
(defn empty-env
  "Returns an empty env"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         'user})
```

Environments track a form's context, the locals that were available to the form, and the namespace the form was analyzed in. The `:context` indicates whether or not the value produced by the form is used (`:ctx/expr` or `:ctx/return`) or discarded (`:ctx/statement`) as in the body of a `do` form. This has implications on the kind of bytecode we'll generate down the line and how we keep the CLR stack balanced. `:locals` is a hash-map of symbols to AST nodes of local binding initialization expressions. This is what connects bindings in `let`, `loop` forms and parameters in `fn` forms to symbols in their bodies. `:ns` tracks the namespace a form was analyzed in and makes var lookup possible.

These environments become embedded in the AST nodes that `tools.analyzer` produces. Additionally, there is a "global environment" that determines all available namespaces and their mappings available to the analyzer that lives in `magic.analyzer/global-env`

```clj
;; ns magic.analyzer
(defn build-ns-map []
  (into {} (mapv #(vector (ns-name %)
                          {:mappings (ns-map %)
                           :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                                                {} (ns-aliases %))
                           :ns       (ns-name %)})
                 (all-ns))))

(defn global-env []
  (atom {:namespaces (build-ns-map)}))
```

## Platform Implementer Bindings
`tools.analyzer` exposes the following dynamic variables for implementers to bind: `macroexpand-1`, `parse` `create-var` and `var?`. It uses them in its internal machinery, but exposes them to implementers to provide customized behavior if needed.

`macroexpand-1` desugars host expressions (e.g. turns `(.method target argument)` into `(. target method argument)`) , expands macros and inline functions and — importantly — participates with MAGIC's intrinsics system to *not* expand invocations that might have intrinsic implementations. Vars like `+` and `*` would expand by default into calls to `Numbers.add` and `Numbers.mul` for performance reasons, because a static method invocation is faster than a var invocation. But MAGIC can emit optimized bytecode inline for many var invocations and avoid *even the overhead of invoking a static method*, so the macroexpander leaves the form as is and lets the intrinsics system pick it up later.

The remaining bindings don't do anything new or interesting interesting.

## AST Nodes
MAGIC's analyzer lives in the `magic.analyzer` namespace, where it wraps `tools.analyzer`. The entry point is `magic.analyzer/analyze`

```clj
user> (pprint (magic.analyzer/analyze :hello))
{:op :const,
 :env {:context :ctx/expr, :locals {}, :ns user},
 :type :keyword,
 :literal? true,
 :val :hello,
 :form :hello,
 :top-level true}
```

The simplest usage takes a single form and provides an Abstract Syntax Tree describing it. ASTs here are not special types or data structures, but simple Clojure hash-maps. The analysis of the literal keyword `:hello` reveals the kind of form it is, `:op :const`, the environment it was analyzed in, `:env {:context :ctx/expr, :locals {}, :ns user}`, and other information.

Invoking a var produces a bigger hash-map.

```clj
user> (pprint (magic.analyzer/analyze '(str 1)))
{:op :invoke,
 :form (str 1),
 :env {:context :ctx/expr, :locals {}, :ns user},
 :fn
 {:op :var,
  :assignable? false,
  :var #'clojure.core/str,
  :meta
  {:added "1.0",
   :ns #object[Namespace 0xcd2817c6 "clojure.core"],
   :name str,
   :file "clojure/core.clj",
   :static true,
   :column 2,
   :line 543,
   :tag System.String,
   :arglists ([] [x] [x & ys]),
   :doc
   "With no args, returns the empty string. With one arg x, returns\n  x.toString().  (str nil) returns the empty string. With more than\n  one arg, returns the concatenation of the str values of the args."},
  :env {:context :ctx/expr, :locals {}, :ns user},
  :form str},
 :args
 [{:op :const,
   :env {:context :ctx/expr, :locals {}, :ns user},
   :type :number,
   :literal? true,
   :val 1,
   :form 1}],
 :children [:fn :args],
 :top-level true}
```

A few keys are familiar, `:op`, `:form`, and `:env`, but there are new keys specific to invocation, namely `:fn` and `:args`. 

In fact, `tools.analyzer`'s nodes only make a few guarantees. From the docstring:

```
   Every node in the AST is a map that is *guaranteed* to have the following keys:
   * :op   a keyword describing the AST node
   * :form the form represented by the AST node
   * :env  the environment map of the AST node

   Additionally if the AST node contains sub-nodes, it is guaranteed to have:
   * :children a vector of the keys of the AST node mapping to the sub-nodes,
               ordered, when that makes sense
```

This is why the `:const` node earlier has no `:children` but the `:invoke` expression does, because its `:fn` and `:args` keys are actually deeper AST nodes or vectors of AST nodes with their own `:op` keys.

Beyond that, the structure of an AST node is *completely dynamic*. This approach is has all the benefits that Clojure data gives you with enough regularity to run generic recursive walks through a deeply nested tree.

There isn't any documentation on the contents of the different AST node types, and I personally learned about the structure of different the AST nodes by experimenting in the REPL.

## MAGIC Passes
On its own, `tools.analyzer` only analyzes *most* of Clojure, namely, the subset of Clojure that is not *host-specific*.

Consider the analysis of [`DateTime`](https://msdn.microsoft.com/en-us/library/system.datetime(v=vs.110).aspx)

```clj
user> (pprint (magic.analyzer/analyze 'DateTime))
{:op :const,
 :env {:context :ctx/expr, :locals {}, :ns user},
 :form System.DateTime,
 :top-level true,
 :children [],
 :type :class,
 :literal? true,
 :val System.DateTime}
```

MAGIC determines this to be a `:const` AST node and resolves `DateTime` to the C# type [`System.DateTime`](https://msdn.microsoft.com/en-us/library/system.datetime(v=vs.110).aspx), something future compiler phases will need to generate correct bytecode.

This information comes from additional analysis passes that MAGIC builds on top of `tools.analyzer`. We can turn them off and see what this form would analyze to by default.

```clj
user> (pprint (magic.analyzer/analyze 'DateTime (magic.analyzer/empty-env) identity))
{:op :maybe-class,
 :class DateTime,
 :env {:context :ctx/expr, :locals {}, :ns user},
 :form DateTime,
 :top-level true}
```

The `:op` is now `:maybe-class` and the `:class` is just the symbol `DateTime`. This is what host interop analysis looks by default to `tools.analyzer`: It has no idea what this is! This is by design. `tools.analyzer`'s approach is to provide complete analysis for host-agnostic forms, generic analysis for host-specific forms, and then facilities to *schedule additional passes* on top of that.

`tools.analyzer` can schedules passes and descend into the basic AST it initially analyzed, using the `:children` key on AST nodes to know which nodes to recurse into. It replaces AST nodes that it finds on the way down the tree and on the way up the tree depending on the scheduled passes.

An analysis pass is just a Clojure function with additional metadata. The function responsible for the correct analysis of type literals in MAGIC is `magic.analyzer.analyze-host-forms/analyze-type`

```clj
;; ns magic.analyzer.analyze-host-forms
(defn analyze-type
  "Analyze foo into a type"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [op children class] :as ast}]
  (if (= :maybe-class op)
    (let [target-type (ensure-class (name class) (:form ast))]
      (merge (dissoc ast :class)
             {:children (vec (remove #(= % :class) children))}
             {:op :const
              :type :class
              :literal? true
              :val target-type
              :form target-type}))
    ast))
```

The function takes an AST node as input and is returns a new AST node to replace it. In this case, if the `:op` key is `:maybe-class`, we attempt to resolve the type and return a new `:const` AST node. Otherwise, we return the original AST unmodified.

The metadata determines where in the walk this happens. `tools.analyzer` uses this information for optimizations, and there are cases when the order of passes matters. In this case, `analyze-type` happens after `uniquify-locals`, which is itself a pass.

MAGIC's passes are collected in `magic.analyzer/default-passes`

```clj
user> (pprint magic.analyzer/default-passes)
#{#'magic.analyzer.analyze-host-forms/analyze-type
  #'clojure.tools.analyzer.passes.elide-meta/elide-meta
  #'magic.analyzer.analyze-host-forms/analyze-host-interop
  #'magic.analyzer.analyze-host-forms/analyze-host-call
  #'magic.analyzer.analyze-host-forms/analyze-constructor
  #'magic.analyzer/increment-arg-ids
  #'magic.analyzer.analyze-host-forms/analyze-host-field
  #'magic.analyzer.analyze-host-forms/analyze-byref
  #'magic.analyzer.novel/csharp-operators #'magic.analyzer/collect-vars
  #'clojure.tools.analyzer.passes.uniquify/uniquify-locals
  #'magic.analyzer/tag-catch-locals
  #'magic.analyzer.novel/generic-type-syntax
  #'magic.analyzer.intrinsics/analyze
  #'clojure.tools.analyzer.passes.warn-earmuff/warn-earmuff
  #'magic.analyzer.literal-reinterpretation/analyze}
```

Some passes implement features, like `magic.analyzer.novel/csharp-operators` which analyzes `(Vector3/+ a b)` to the equivalent but clunkier `(Vector3/op_Addition a b)`. Others implement optimizations, like `magic.analyzer.literal-reinterpretation/analyze` which reinterprets literals when possible to avoid needless casts and `magic.analyzer.intrinsics/analyze` which implements MAGIC's intrinsic machinery.

This set of passes is fed to `clojure.tools.analyzer.passes/schedule` which constructs a function that takes an AST node, runs the passes in the set in the correct order, and returns a new AST node. With the correct bindings in place and a global environment established, this is all we need to produce an AST that is ready to be turned into symbolic bytecode.

```clj
;; ns magic.analyzer
(def scheduled-default-passes
  (schedule default-passes))

(defn run-passes [ast]
  (scheduled-default-passes ast))

(defn analyze
  ([form] (analyze form (empty-env)))
  ([form env] (analyze form env run-passes))
  ([form env passes-fn]
   (binding [ana/macroexpand-1 macroexpand-1
             ana/create-var    (fn [sym env]
                                 (doto (intern (:ns env) sym)
                                   (reset-meta! (meta sym))))
             ana/parse         parse
             ana/var?          var?]
     (with-env (global-env) (passes-fn (ana/analyze form env))))))
```

🎩✨
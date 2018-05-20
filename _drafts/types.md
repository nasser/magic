---
layout: post
title: "The Analyzer: Types"
---

Every expression in Clojure has a type, and MAGIC uses the `magic.analyzer.types/ast-type` multimethod to figure out the type of an analyzed expression. It takes an AST node and dispatches on its `:op` key to return the type of that node's expression. This information is used by later compiler stages to generate correct (and, ideally, optimal) bytecode.

In the examples below, the REPL interactions are all in the `user` namespace wih `ast-type` `refer`ed from `magic.analyzer.types` and `magic.analyzer` aliased as `ana`. Code snippets that are not REPL interactions are taken from the `magic.analyzer.types` namespace.

## Basic Expression Types

To get a feel for this system, examples are helpful. Some expressions are straightforward. For example, literal vectors, sets, and maps are of all types `IPersistentVector`, `IPersistentSet`, and `IPersistentMap` respectively.

```clj
(defmethod ast-type :vector [ast]
  clojure.lang.IPersistentVector)

(defmethod ast-type :set [ast]
  clojure.lang.IPersistentSet)

(defmethod ast-type :map [ast]
  clojure.lang.IPersistentMap)
```

```clj
user> (ast-type (ana/analyze '[1 2 3])))
clojure.lang.IPersistentVector
user> (ast-type (ana/analyze '[1 :hello 2 :world 3])))
clojure.lang.IPersistentVector
user> (ast-type (ana/analyze '{:foo 1 :bar 2})))
clojure.lang.IPersistentMap
```

The value of a `do` expression is the value of the last expression in its body, so it follows that the type of a `do` expression is the type of its last expression, too. `tools.analyzer` puts the last expression in the `:ret` key.

```clj
(defmethod ast-type :do
  [{:keys [ret] :as ast}]
  (ast-type ret))
```

```clj
user> (ast-type (ana/analyze '(do 1 2 3)))
System.Int64
user> (ast-type (ana/analyze '(do 1 2 "3")))
System.String
```

For interop forms, MAGIC's analysis passes place CLR reflection types in the AST notes that makes type analysis very easy.

```clj
(defmethod ast-type :static-method [ast]
  (-> ast :method .ReturnType))

(defmethod ast-type :instance-method [ast]
  (-> ast :method .ReturnType))

(defmethod ast-type :static-property [ast]
  (-> ast :property .PropertyType))

(defmethod ast-type :instance-property [ast]
  (-> ast :property .PropertyType))

(defmethod ast-type :static-field [ast]
  (-> ast :field .FieldType))

(defmethod ast-type :instance-field [ast]
  (-> ast :field .FieldType))
```

```clj
user> (ast-type (ana/analyze '(Math/Sin 123)))
System.Double
user> (ast-type (ana/analyze '(Math/Abs 10.5)))
System.Double
user> (ast-type (ana/analyze '(Math/Abs 10)))
System.Int64
user> (ast-type (ana/analyze 'DateTime/Now))
System.DateTime
user> (ast-type (ana/analyze '(.Hour DateTime/Now)))
System.Int32
user> (ast-type (ana/analyze '(.Day DateTime/Now)))
System.Int32
```

## Bindings and Locals

A binding's type is either its tag (if present), the type of the expression it was initialized with, or `Object`.

```clj
(defmethod ast-type :binding [ast]
  (or
    (if-let [tag (-> ast :form meta :tag)]
      (if (symbol? tag)
        (resolve tag)
        tag))
    (if-let [init (:init ast)]
      (ast-type init))
    Object))
```

```clj
user> (-> '(let [a "const"] a) ana/analyze :bindings first ast-type)
System.String
user> (-> '(let [a DateTime/Now] a) ana/analyze :bindings first ast-type)
System.DateTime
user> (-> '(let [a (Math/Sin (.Hour DateTime/Now))] a) ana/analyze :bindings first ast-type)
System.Double
```

Note that the tag overrides anything else. If a tag is present, MAGIC will trust that blindly and try to generate a conversion down the line, which could lead to a compiler error.

```clj
user> (-> '(let [^Single a 10] a) ana/analyze :bindings first ast-type)
System.Single
user> (-> '(let [^String a 10] a) ana/analyze :bindings first ast-type)
System.String ;; ???
user> (-> '(let [^DateTime a 10] a) ana/analyze :bindings first ast-type)
System.DateTime ;; ???
```

All of the above bindings are initialized with `10`, a long, but they're all tagged differently. What does it mean to initialize a binding with an expression of one type, but tag it with another type, especially bizarre ones in this case like `String` and `DateTime`? MAGIC will try to emit conversion bytecode when possible and will fail at compile time if it cannot. This means that you can use tags in MAGIC to *ensure* that a binding will have a specific type at runtime.

```clj
user> (-> '(let [^Single a 10] a) ana/analyze magic/compile pprint)
[([{:mage.core/opcode ldc.i8, :mage.core/argument 10}
   {:mage.core/opcode conv.r4} ;; <== convert real 4 bytes (convert to a float)
   {:mage.core/opcode stloc,
    :mage.core/argument
    {:mage.core/local "a__#0", :mage.core/type System.Single}}])
 nil
 [()
  {:mage.core/opcode ldloc,
   :mage.core/argument
   {:mage.core/local "a__#0", :mage.core/type System.Single}}
  nil]]
user> (-> '(let [^String a 10] a) ana/analyze magic/compile pprint)
  [([{:mage.core/opcode ldc.i8, :mage.core/argument 10}
     [[{:mage.core/opcode stloc,
        :mage.core/argument
        {:mage.core/local local8616, :mage.core/type System.Int64}}
       {:mage.core/opcode ldloca,
        :mage.core/argument
        {:mage.core/local local8616, :mage.core/type System.Int64}}]
      {:mage.core/opcode call,
       :mage.core/argument
       #object[MonoMethod 0x5ac1ac06 "System.String ToString()"]}] ;; <== convert to string by calling ToString
     {:mage.core/opcode stloc,
      :mage.core/argument
      {:mage.core/local "a__#0", :mage.core/type System.String}}])
   nil
   [()
    {:mage.core/opcode ldloc,
     :mage.core/argument
     {:mage.core/local "a__#0", :mage.core/type System.String}}
    nil]]
user> (-> '(let [^DateTime a 10] a) ana/analyze magic/compile pprint)
Exception System.Exception: Cannot convert System.Int64 to System.DateTime
```

## Mutation

`set!` gets a little trickier. Clojure's semantics dictate that `set!` returns the type of the value it assigns, but, as an optimization, MAGIC analyzes it to a void type if it is in a statement context, meaning its value is never used. This avoids needless local variables down the line.

```clj
(defmethod ast-type :set!
  [{:keys [val] {:keys [context]} :env}]
  (if (= context :ctx/statement)
    System.Void
    (ast-type val)))
```

## Differences

MAGIC differs from ClojureJVM/ClojureCLR in the way that it handles primitive types and value types. Consider the following

```clj
;; MAGIC
user> (let [^int a 1] a)
1

;; ClojureJVM
user=> (let [^int a 1] a)

CompilerException java.lang.UnsupportedOperationException: Can't type hint a local with a primitive initializer, compiling:...
```

The primary goal of MAGIC's type machinery is to avoid [*boxing*][boxing], the implicit conversion of [*value types*][value type] allocated on the stack into *reference* types allocated in the heap. MAGIC also works to avoid *reflection* and catch (very) basic type errors, but boxing has been the main focus.

Boxing is a problem because when you move a value from the stack to the heap, the CLR allocator needs to find memory for it and that memory needs to be garbage collected eventually. Game development requires tight inner loops to be free of allocator/collector pressure or "zero-alloc". Pervasive boxing makes that impossible. In an environment like Unity, with a particularly bad garbage collector implementation, this is more needed than ever.

What causes boxing?

<!--
CLR/JVM differences
fn parameters
  Function<>
dynamic lang + static vm interop = types
unanswered questions
-->

[boxing]: https://en.wikipedia.org/wiki/Object_type_(object-oriented_programming)#Boxing
[value type]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/value-types

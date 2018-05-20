---
layout: post
title: The Compiler
---

The heart of MAGIC is its compiler. It takes Abstract Syntax Trees from [the analyzer](/2017/10/16/the-analyzer.html) and turns them into *symbolic bytecode* emission to disk or memory by the emitter, MAGE.

The MAGIC compiler is actually a symphony of smaller compilers. Each compiler is a function that takes an AST node and a map of compilers and returns the bytecode that implements the AST node. The map of compilers has analyzer `:op` keywords as keys and compilers as values. There is a map of built in compilers that is passed in at the start of compilation

```clj
(def base-compilers
  {:const               #'const-compiler
   :do                  #'do-compiler
   :vector              #'vector-compiler
   :set                 #'set-compiler
   ;; ...
   :instance-method     #'instance-method-compiler
   :initobj             #'initobj-compiler
   :new                 #'new-compiler
   :intrinsic           #'intrinsic-compiler})
```

This map is threaded through the compilation. This association used to be implemented as a multimethod, but passing a map this way enables important tricks that MAGIC takes advantage of when compiling the trickier parts of Clojure.

## Simple Compilers

The simplest compiler is `const-compiler`. It takes a literal value and compiles them to instructions that load them onto the CLR evaluation stack. Literal values analyze to AST nodes with an `:op` of `:const`.

```clj
user> (-> 60 magic.analyzer/analyze pprint)
{:op :const,
 :env {:context :ctx/expr, :locals {}, :ns user},
 :type :number,
 :literal? true,
 :val 60,
 :form 60,
 :top-level true}

user> (-> "hello" magic.analyzer/analyze pprint)
{:op :const,
 :env {:context :ctx/expr, :locals {}, :ns user},
 :type :string,
 :literal? true,
 :val "hello",
 :form "hello",
 :top-level true}
```

The other interesting key is `:val`, which is the compile time value of the literal. The job of `const-compiler` is to turn these AST nodes into symbolic bytecode. It works like this.

```clj
user> (-> nil magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldnull}

user> (-> "hello" magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldstr, :mage.core/argument "hello"}

user> (-> (int 0) magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldc.i4.0}

user> (-> (int 1) magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldc.i4.1}

user> (-> (int -1) magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldc.i4.m1}

user> (-> (int 60) magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldc.i4.s, :mage.core/argument 60}

user> (-> 60 magic.analyzer/analyze magic.core/compile)
{:mage.core/opcode ldc.i8, :mage.core/argument 60}
```

The implementation is large due to the number of type of literals MAGIC needs to support, but the core of `const-compiler` is a one-liner and the basics are covered by this snippet.

```clj
(defn load-integer [k]
  (cond
    (= k 0)  (il/ldc-i4-0)
    (= k 1)  (il/ldc-i4-1)
    (= k -1) (il/ldc-i4-m1)
    (< k 128) (il/ldc-i4-s (byte k))
    :else (il/ldc-i4 (int k))))

(defmulti load-constant type)

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " (type k)))

(defmethod load-constant nil [k]
  (il/ldnull))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (load-integer k))

(defmethod load-constant Int64 [k]
  (il/ldc-i8 k))

(defmethod load-constant UInt64 [k]
  (il/ldc-i8 k))

(defmethod load-constant UInt32 [k]
  (load-integer k))

;; ...

(defn const-compiler
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast} compilers]
  (load-constant val))
```

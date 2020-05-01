(ns magic.api
  (:refer-clojure :exclude [compile load defn eval])
  (:require [magic.analyzer :as ana]
            [magic.analyzer.types :refer [tag ast-type]]
            [magic.core :as magic]
            [mage.core :as il]
            magic.intrinsics
            [magic.spells
             [lift-vars :refer [lift-vars]]
             [dynamic-interop :refer [dynamic-interop]]])
  (:import [clojure.lang RT]
           [System.Reflection.Emit AssemblyBuilderAccess]))

(clojure.core/defn module* [name]
  (->
   (.. AppDomain CurrentDomain
       (DefineDynamicAssembly
        (AssemblyName. name)
        AssemblyBuilderAccess/RunAndSave))
   (.DefineDynamicModule (str name ".dll"))))

(clojure.core/defn compile-asm
  ([exprs]
   (compile-asm "magic.compile" exprs))
  ([asm-name exprs]
   (compile-asm asm-name (magic/get-compilers) exprs))
  ([asm-name compilers exprs]
   (->> (map #(-> % ana/analyze (magic/compile compilers))
             exprs)
        (il/assembly+module asm-name)
        il/emit!
        ::il/assembly-builder)))

(clojure.core/defn compile-fn
  "Compile fn form using MAGIC, emit binary to current ClojureCLR compilation context
   and return the IFn instance."
  ([expr] (compile-fn expr (magic/get-compilers)))
  ([expr compilers]
   (-> expr
       ana/analyze
       (magic/compile compilers)
       il/emit!
       ::il/type-builder
       Activator/CreateInstance)))

(clojure.core/defn compile-fn-ctor [expr]
  (->> (compile-fn expr)
       .GetType
       .Name
       symbol
       (list 'new)))

(clojure.core/defn eval [expr]
  (binding [magic/*module* (module* "eval")]
    (let [ast (ana/analyze expr)
          bc (magic/compile ast)]
      (->> (il/type
            (str (gensym "magic$eval$"))
            (il/method
             "eval"
             Object []
             [bc
              (magic/convert (ast-type ast) Object)
              (il/ret)]))
           (il/emit! {::il/module-builder magic/*module*})
           ::il/type-builders ;;  TODO MAGE bug type-builder should be available here
           vals
           first
           Activator/CreateInstance
           .eval))))


(defmacro defn
  "Compile a function using MAGIC. Useable from namespaces
   compiled by ClojureCLR."
  [name args & body]
  (let [form (list* 'fn name args body)]
    `(def ~name
       ~(compile-fn form))))

#_
(defmacro faster
  "Compile body of expression using MAGIC and emit a well-typed call site
   instead. Useable from namespaces compiled by ClojureCLR, supports closures."
  [& body]
  (let [ks (keys &env)
        vs (vals &env)
        types (map #(or (tag %1)
                        (and (.HasClrType %2) (.ClrType %2))
                        Object)
                   ks vs)
        ftype (symbol (faster-type ks types (list* 'do body)))]
    (.importClass *ns* (RT/classForName (str ftype)))
    `(. ~ftype ~'invoke ~@ks)))

(clojure.core/defn bind-spells! [spells]
  (alter-var-root #'magic/*spells* (constantly spells)))

(clojure.core/defn bind-basic-spells! []
  (bind-spells! [dynamic-interop]))

;; yolo
(bind-spells! [dynamic-interop])
(ns magic.api
  (:refer-clojure :exclude [compile defn])
  (:require [clojure.core :as c]
            [magic.analyzer :as ana]
            [magic.analyzer.types :refer [tag]]
            [magic.core :as magic]
            [magic.faster :refer [faster-type]]
            [mage.core :as il]
            [magic.spells
             [lift-vars :refer [lift-vars]]])
  (:import [clojure.lang RT]))

(c/defn compile-asm
  ([exprs]
   (compile-asm "magic.compile" exprs))
  ([asm-name exprs]
   (compile-asm asm-name (magic/get-symbolizers) exprs))
  ([asm-name symbolizers exprs]
   (->> (map #(-> % ana/analyze (magic/symbolize symbolizers))
             exprs)
        (il/assembly+module asm-name)
        il/emit!
        ::il/assembly-builder)))

(c/defn compile-fn
  "Compile fn form using MAGIC, emit binary to current ClojureCLR compilation context
   and return constructor form."
  ([expr] (compile-fn expr (magic/get-symbolizers)))
  ([expr symbolizers]
   (->> (-> expr
            ana/analyze
            (magic/symbolize symbolizers))
        il/emit!
        ::il/type-builder
        .Name
        symbol
        (list 'new))))

(defmacro defn
  "Compile a function using MAGIC. Useable from namespaces
   compiled by ClojureCLR."
  [name args & body]
  (let [form (list* 'fn name args body)]
    `(def ~name
       ~(compile-fn form))))

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

(c/defn bind-spells! [spells]
  (alter-var-root #'magic/*spells* (constantly spells)))

(c/defn bind-basic-spells! []
  (bind-spells! [lift-vars]))
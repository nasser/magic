(ns magic.core
  (:refer-clojure :exclude [compile resolve])
  (:require [mage.core :as il]
            [magic.analyzer :as ana]
            [clojure.string :as string])
  (:import [clojure.lang RT]
           [System.IO FileInfo Path]
           [System.Reflection TypeAttributes MethodAttributes FieldAttributes FieldInfo MethodInfo PropertyInfo]
           System.AppDomain))

(defmulti clr-type :op)

(defmethod clr-type :default [ast]
  (throw! "clr-type not implemented for " (pr-str ast)))



(def base-symbolizers
  {:const           #'literal-symbolizer
   :vector          #'vector-symbolizer
   :set             #'set-symbolizer
   :map             #'map-symbolizer
   :invoke          #'invoke-symbolizer
   :var             #'var-symbolizer
   :the-var         #'var-symbolizer
   :do              #'do-symbolizer
   :fn              #'fn-symbolizer
   :fn-method       #'fn-method-symbolizer
   :maybe-host-form #'host-form-symbolizer
   :host-interop    #'host-interop-symbolizer
   :new             #'new-symbolizer
   :let             #'let-symbolizer
   :binding         #'binding-symbolizer
   :local           #'local-symbolizer
   :if              #'if-symbolizer})

(defn ast->symbolizer [ast symbolizers]
  (or (-> ast :op symbolizers)
      (throw (Exception. (str "No symbolizer for " (pr-str (or  (:op ast)
                                                               ast)))))))

(defn symbolize [ast symbolizers]
  (if-let [symbolizer (ast->symbolizer ast symbolizers)]
    (symbolizer ast symbolizers)))

(defn compile-fn [expr]
  (let [asm-name "magic.tests"]
    (-> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (symbolize (ana/ast expr) base-symbolizers)))
        il/emit!
        :mage.core/assembly-builder
        .GetTypes
        first
        Activator/CreateInstance
        )))
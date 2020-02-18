(ns magic.spells.dynamic-interop
  (:use clojure.pprint)
  (:require [magic.core :as magic]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [System.Reflection CallingConventions FieldAttributes BindingFlags MethodAttributes]))

(def invokeZeroArityMemberMethod (interop/method Magic.Dispatch "InvokeZeroArityMember" Object String))
(def invokeInstanceMethod (interop/method Magic.Dispatch "InvokeMember" Object String (clojure.lang.RT/classForName "System.Object[]")))

(defn dynamic-interop [compilers]
  (assoc compilers
         :dynamic-zero-arity
         (fn dynamic-zero-arity-compiler
           [{:keys [m-or-f form raw-forms target] :as ast} compilers]
           (when *warn-on-reflection*
             ;; TODO get file and line number information in here
             (println (str "Reflection warning - reference to field " m-or-f " can't be resolved (target class is unknown)"))
             (println (str "  in form " (first raw-forms) ", expanded to " form ".")))
           [(magic/compile target compilers)
            (il/ldstr (str m-or-f))
            (il/call invokeZeroArityMemberMethod)])
         :dynamic-method
         (fn dynamic-method-compiler
           [{:keys [method args form raw-forms target] :as ast} compilers]
           (when *warn-on-reflection*
             ;; TODO get file and line number information in here
             (println (str "Reflection warning - reference to method " method " can't be resolved (target class is unknown)"))
             (println (str "  in form " (first raw-forms) ", expanded to " form ".")))
           [(magic/compile target compilers)
            (il/ldstr (str method))
            (magic/prepare-array args compilers)
            (il/call invokeInstanceMethod)])))
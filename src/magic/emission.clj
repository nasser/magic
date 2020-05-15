(ns magic.emission
  (:import [System.Reflection AssemblyName]
           [System.Reflection.Emit AssemblyBuilderAccess]))

(defn fresh-module 
  "Creates a fresh ModuleBuilder, suitable for binding to *module*"
  [name]
  (->
   (.. AppDomain CurrentDomain
       (DefineDynamicAssembly
        (AssemblyName. name)
        AssemblyBuilderAccess/RunAndSave))
   (.DefineDynamicModule (str name ".dll"))))

(def ^:dynamic
  *module*
  "The module that byecode is being emitted into. This generally needs to be
   bound before any analysis or compilation can happen. Pulled into its own 
   namespace to be visible to magic.core and magic.analyzer.types."
  nil)
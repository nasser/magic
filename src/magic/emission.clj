(ns magic.emission
  (:import [System.Reflection AssemblyName]
           [System.Reflection.Emit AssemblyBuilderAccess]))

;; this differs between .NET Framework and .NET Standard/Core
(def assemby-builder-access-values (-> AssemblyBuilderAccess Enum/GetNames set))

;; we use RunAndSave if its available (meaning we're on .NET Framework)
;; and fall back to Run otherwise (meaning we're on .NET Standard/Core)
;; we avoid emitting a static reference to either to that AOT'd assemblies
;; can work in either context
(def assemby-builder-access 
  (Enum/Parse AssemblyBuilderAccess
              (or (assemby-builder-access-values "RunAndSave")
                  (assemby-builder-access-values "Run"))))

(defn fresh-module 
  "Creates a fresh ModuleBuilder, suitable for binding to *module*"
  [name]
  (->
   (Magic.Emission/DefineDynamicAssembly 
    (AssemblyName. name)
    assemby-builder-access)
   (.DefineDynamicModule (str name ".dll"))))

(def ^:dynamic
  *module*
  "The module that byecode is being emitted into. This generally needs to be
   bound before any analysis or compilation can happen. Pulled into its own 
   namespace to be visible to magic.core and magic.analyzer.types."
  nil)
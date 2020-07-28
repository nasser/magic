(ns magic.analyzer.binder
  (:import Magic.Binder [System.Reflection BindingFlags MethodBase]))

(def binder Binder/Shared)

(defn select-method
  ([methods arg-types]
   (select-method methods arg-types BindingFlags/Default nil))
  ([methods arg-types flags modifiers]
   (try
     (.SelectMethod
      binder
      flags
      (into-array MethodBase methods)
      (into-array Type arg-types)
      modifiers)
     (catch System.Reflection.AmbiguousMatchException e
       ;; TODO is it ok if this is silent?
       nil))))
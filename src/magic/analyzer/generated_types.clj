(ns magic.analyzer.generated-types
  (:import [System.Reflection.TypeAttributes]))

(def public TypeAttributes/Public)
(def interface (enum-or 
                TypeAttributes/Public
                TypeAttributes/Abstract
                TypeAttributes/Interface))
(def public-sealed (enum-or TypeAttributes/Public TypeAttributes/Sealed))

(defn fresh-type [module-builder name super interfaces attributes]
  (.DefineType
   module-builder name attributes super (into-array Type interfaces)))

(defn deftype-type [module-builder name interfaces]
  (fresh-type module-builder name Object interfaces public))

(defn gen-interface-type [module-builder name interfaces]
  (fresh-type module-builder name Object interfaces interface))

(defn fn-type [module-builder name interfaces]
  (fresh-type module-builder name clojure.lang.AFunction interfaces public))

(defn proxy-type [module-builder super interfaces]
  (fresh-type module-builder (str (gensym "proxy")) super interfaces public))

(defn reify-type [module-builder interfaces]
  (fresh-type
   module-builder (str (gensym "reify")) Object interfaces public-sealed))
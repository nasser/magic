(ns magic.analyzer.generated-types
  (:require [magic.analyzer.types :as types])
  (:import [System.Reflection TypeAttributes]))

(def ^:dynamic
  *reusable-types*
  nil)

(def public TypeAttributes/Public)
(def interface (enum-or
                TypeAttributes/Public
                TypeAttributes/Abstract
                TypeAttributes/Interface))
(def public-sealed (enum-or TypeAttributes/Public TypeAttributes/Sealed))

(defn type-match [type super interfaces attributes]
  (when (and (= super (.BaseType type))
             (= attributes (.Attributes type))
             (= (into #{} interfaces)
                (into #{} (.GetInterfaces type))))
    type))

(defn define-new-type [module-builder name super interfaces attributes]
  (types/type-lookup-cache-evict! name)
  (if module-builder
    (.DefineType
     module-builder name attributes super (into-array Type interfaces))
    (throw (Exception. (str "no module builder provided when defining new type " 
                            name
                            ", was magic.emission/*module* bound?")))))

(defn fresh-type [module-builder name super interfaces attributes]
  (if *reusable-types*
    (if-let [type (some #(type-match % super interfaces attributes) @*reusable-types*)]
      (do 
        (swap! *reusable-types* disj type)
        type)
      (do
        (println "[fresh-type] making new type")
        (define-new-type
          module-builder name super interfaces attributes)))
    (define-new-type
      module-builder name super interfaces attributes)))

(defn deftype-type [module-builder name interfaces]
  (fresh-type module-builder name Object interfaces public))

(defn gen-interface-type [module-builder name interfaces]
  (fresh-type module-builder name Object interfaces interface))

(defn fn-type [module-builder name interfaces]
  (fresh-type module-builder name clojure.lang.AFunction interfaces public))

(defn variadic-fn-type [module-builder name interfaces]
  (fresh-type module-builder name clojure.lang.RestFn interfaces public))

(defn proxy-type [module-builder name super interfaces]
  (fresh-type module-builder name super interfaces public))

(defn reify-type [module-builder name interfaces]
  (fresh-type module-builder name Object interfaces public-sealed))
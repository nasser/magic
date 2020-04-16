(ns magic.faster
  (:require [clojure.tools.analyzer.passes :refer [schedule]]
            [magic.analyzer :as ana]
            [magic.analyzer.types :refer [ast-type non-void-ast-type tag]]
            [magic.core :as magic]
            [mage.core :as il])
  (:import [clojure.lang RT]
           [System.Reflection
            MethodAttributes]))

(defn static-type-fn
  "Symbolize :fn to a simple static type without interfaces,
  HasArity methods, or constructors"
  [{:keys [local methods] :as ast} compilers]
  (il/type
    (ana/gen-fn-name (:form local))
    (map #(magic/compile % compilers) methods)))

(defn typed-static-invoke-fn-method
  "Symbolize :fn-method to a single, well typed static method"
  [{:keys [body params] {:keys [ret]} :body} compilers]
  (let [param-types (mapv ast-type params)
        return-type (non-void-ast-type ret)]
    (il/method
      "invoke"
      (enum-or MethodAttributes/Public MethodAttributes/Static)
      return-type param-types
      [(magic/compile body compilers)
       (magic/convert (ast-type ret) return-type)
       (il/ret)])))

(def faster-passes
  (disj ana/default-passes
        #'ana/increment-arg-ids))

(def scheduled-faster-passes
  (schedule faster-passes))

;; TODO ::il/type is kind of lame, use ::il/name maybe?
(defn faster-type [args-names args-types body]
  (let [faster-compilers
        (merge magic/base-compilers
               {:fn static-type-fn
                :fn-method typed-static-invoke-fn-method
                :local magic/local-compiler})
        name (symbol (str "--" (gensym "faster--body") "--"))
        wrapped-body `(clojure.core/fn
                        ~name
                        ~(->> args-names
                              (map #(vary-meta %2 assoc :tag %1)
                                   args-types)
                              vec)
                        ~body)
        body-il (-> wrapped-body
                    (ana/analyze (ana/empty-env) scheduled-faster-passes)
                    (magic/compile (magic/get-compilers faster-compilers)))]
    (il/emit! body-il)
    (::il/type body-il)))
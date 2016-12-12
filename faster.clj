(ns magic.faster
  (:require [clojure.tools.analyzer.clr :as ana]
            [mage.core :as il])
  (:import [System.Reflection
            TypeAttributes
            MethodAttributes
            FieldAttributes
            FieldInfo
            MethodInfo
            PropertyInfo]))

(defn faster-type [symbolizers args-names args-types return body]
  (let [name (gensym "Faster")
        wrapped-body `(clojure.core/fn
                        ~(->> args-names
                              (map #(vary-meta %2 assoc :tag %1)
                                   args-types)
                              vec)
                        ~body)
        body-ast (ana/analyze wrapped-body)
        body-expr (-> body-ast :methods first :body)
        body-il (symbolize body-expr symbolizers)
        body-il (->> body-il flatten (remove nil?) peephole)
        type-il
        (il/assembly
          "faster"
          (il/module
            (str "faster.dll")
            (mage.core/type
              (str name)
              TypeAttributes/Public []
              Object
              [(il/method
                 "Invoke"
                 (enum-or MethodAttributes/Public
                          MethodAttributes/Static)
                 return
                 (vec args-types)
                 [body-il   
                  (convert (clr-type (body-expr :ret)) return)
                  (il/ret)]
                 )])))]
    (il/emit! type-il)
    name))

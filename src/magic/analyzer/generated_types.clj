(ns magic.analyzer.generated-types
  (:import [System.Reflection TypeAttributes]))

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
  (fresh-type module-builder name clojure.lang.AFn interfaces public))

(defn variadic-fn-type [module-builder name interfaces]
  (fresh-type module-builder name clojure.lang.RestFn interfaces public))

(defn proxy-type [module-builder super interfaces]
  (fresh-type module-builder (str (gensym "proxy")) super interfaces public))

(defn reify-type [module-builder interfaces]
  (fresh-type
   module-builder (str (gensym "reify")) Object interfaces public-sealed))

#_
(defn bind-interface-method [f name params candidate-methods]
  (let [name (str name)
        params* (drop 1 params) ;; reify uses explicit this
        [interface-name method-name]
        (if (string/includes? name ".")
          (let [last-dot (string/last-index-of name ".")]
            [(subs name 0 last-dot)
             (subs name (inc last-dot))])
          [nil name])
        candidate-methods (filter #(= method-name (.Name %)) candidate-methods)
        candidate-methods (if interface-name
                            (filter #(= interface-name (.. % DeclaringType FullName)) candidate-methods)
                            candidate-methods)]
    (if-let [best-method (select-method candidate-methods (map ast-type params*))]
      (let [hinted-params (mapv #(update %1 :form vary-meta assoc :tag %2) params (concat [reify-type] (map #(.ParameterType %) (.GetParameters best-method))))]
        (assoc f
               :params hinted-params
               :source-method best-method
               :reify-type reify-type))
      (throw (ex-info "no match" {:name name :params (map ast-type params)})))))
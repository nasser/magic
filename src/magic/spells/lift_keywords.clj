(ns magic.spells.lift-keywords
  (:require [magic.core :as magic]
            [mage.core :as il])
  (:import [System.Reflection CallingConventions FieldAttributes MethodAttributes]))

(defn kw-name [v]
  (-> (str (.Namespace v) "$" (name v))
      munge
      (.Replace "." "_")))

(def field-attrs
  (enum-or FieldAttributes/Assembly
           FieldAttributes/Static))

(defn kw-field-map [type kws]
  (apply hash-map
         (interleave
          kws
          (->> kws
               (map #(.DefineField type
                                   (kw-name %)
                                   clojure.lang.Keyword
                                   field-attrs))))))

(defn lift-keywords [compilers]
  (update
   compilers
   :fn
   (fn
     [old-fn-compiler]
     (fn lifted-keyword-fn-compiler
       [{:keys [keywords fn-type fn-type-cctor] :as ast} compilers]
       (if-not (or (.IsCreated fn-type)
                   (zero? (count keywords)))
         (let [vars (->> keywords (map :val) (into #{}))
               kw-fields (kw-field-map fn-type vars)
               cctor-il
               (->> vars
                    (map #(vector
                           (magic/load-constant %)
                           (il/stsfld (kw-fields %)))))
               ilg (.GetILGenerator fn-type-cctor)
               specialized-compilers
               (merge compilers
                      {:const
                       (fn lifted-keyword-const-compiler
                         [{:keys [val] :as ast} local-compilers]
                         (if-let [field (kw-fields val)]
                           [(il/ldsfld field)]
                           (magic/compile* ast compilers)))})]
           (reduce (fn [ctx x] (il/emit! ctx x))
                   {::il/ilg ilg}
                   [cctor-il])
           (old-fn-compiler ast specialized-compilers))
         (old-fn-compiler ast compilers))))))
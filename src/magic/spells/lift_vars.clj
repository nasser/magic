(ns magic.spells.lift-vars
  (:require [magic.core :as magic]
            [mage.core :as il])
  (:import [System.Reflection CallingConventions FieldAttributes MethodAttributes]))

(defn var-name [v]
  (-> (str (.Namespace v) "$" (.Symbol v))
      munge
      (.Replace "." "_")))

(def field-attrs
  (enum-or FieldAttributes/Assembly
           FieldAttributes/Static))

(def cctor-attrs
  (enum-or MethodAttributes/Public
           MethodAttributes/Public))

(defn var-field-map [type vars]
  (apply hash-map
         (interleave
          vars
          (->> vars
               (map #(.DefineField type
                                   (var-name %)
                                   clojure.lang.Var
                                   field-attrs))))))

(defn lift-vars [compilers]
  (update
   compilers
   :fn
   (fn
     [old-fn-compiler]
     (fn lifted-var-fn-compiler
       [{:keys [vars fn-type fn-type-cctor] :as ast} compilers]
       (if-not (or (.IsCreated fn-type)
                   (zero? (count vars)))
         (let [vars (->> vars (map :var) (into #{}))
               var-fields (var-field-map fn-type vars)
               cctor-il
               (->> vars
                    (map #(vector
                           (magic/load-var %)
                           (il/stsfld (var-fields %)))))
               ilg (.GetILGenerator fn-type-cctor)
               specialized-compilers
               (merge compilers
                      {:var (fn lifted-var-var-compiler
                              [{:keys [var] :as ast} local-compilers]
                              (if-let [field (var-fields var)]
                                [(il/ldsfld field)
                                 (magic/get-var var)]
                                (magic/compile* ast compilers)))
                       :the-var
                       (fn lifted-var-the-var-compiler
                         [{:keys [var] :as ast} local-compilers]
                         (if-let [field (var-fields var)]
                           [(il/ldsfld field)]
                           (magic/compile* ast compilers)))})]
           (reduce (fn [ctx x] (il/emit! ctx x))
                   {::il/ilg ilg}
                   [cctor-il])
           (old-fn-compiler ast specialized-compilers))
         (old-fn-compiler ast compilers))))))
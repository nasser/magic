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

(defn emit-to-cctor!-and-make-specialized-compilers
  [vars containing-type containing-type-cctor compilers]
  (let [vars (->> vars (map :var) (into #{}))
        var-fields (var-field-map containing-type vars)
        cctor-il
        (->> vars
             (map #(vector
                    (magic/load-var %)
                    (il/stsfld (var-fields %)))))
        ilg (.GetILGenerator containing-type-cctor)
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
    specialized-compilers))

(defn lift-vars [compilers]
  (-> compilers
      (update
       :deftype
       (fn
         [old-deftype-compiler]
         (fn lifted-var-deftype-compiler
           [{:keys [vars deftype-type deftype-type-cctor form] :as ast} compilers]
           (if-not (or (.IsCreated deftype-type)
                       (zero? (count vars)))
             (let [specialized-compilers
                   (emit-to-cctor!-and-make-specialized-compilers
                    vars deftype-type deftype-type-cctor compilers)]
               (old-deftype-compiler ast specialized-compilers))
             (old-deftype-compiler ast compilers)))))
      (update
       :fn
       (fn
         [old-fn-compiler]
         (fn lifted-var-fn-compiler
           [{:keys [vars fn-type fn-type-cctor] :as ast} compilers]
           (if-not (or (.IsCreated fn-type)
                       (zero? (count vars)))
             (let [specialized-compilers
                   (emit-to-cctor!-and-make-specialized-compilers
                    vars fn-type fn-type-cctor compilers)]
               (old-fn-compiler ast specialized-compilers))
             (old-fn-compiler ast compilers)))))))
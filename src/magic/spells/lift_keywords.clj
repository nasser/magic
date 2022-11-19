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

(defn emit-to-cctor!-and-make-specialized-compilers
  [keywords containing-type containing-type-cctor compilers]
  (let [kws (->> keywords (map :val) (into #{}))
        kw-fields (kw-field-map containing-type kws)
        cctor-il
        (->> kws
             (map #(vector
                    (magic/load-constant %)
                    (il/stsfld (kw-fields %)))))
        ilg (.GetILGenerator containing-type-cctor)
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
    specialized-compilers))

(defn lift-keywords [compilers]
  (-> compilers
      (update
       :deftype
       (fn
         [old-deftype-compiler]
         (fn lifted-keyword-deftype-compiler
           [{:keys [keywords deftype-type deftype-type-cctor] :as ast} compilers]
           (if-not (or (and deftype-type (.IsCreated deftype-type))
                       (zero? (count keywords)))
             (let [specialized-compilers
                   (emit-to-cctor!-and-make-specialized-compilers
                    keywords deftype-type deftype-type-cctor compilers)]
               (old-deftype-compiler ast specialized-compilers))
             (old-deftype-compiler ast compilers)))))
      (update
       :fn
       (fn
         [old-fn-compiler]
         (fn lifted-keyword-fn-compiler
           [{:keys [keywords fn-type fn-type-cctor] :as ast} compilers]
           (if-not (or (and fn-type (.IsCreated fn-type))
                       (zero? (count keywords)))
             (let [specialized-compilers
                   (emit-to-cctor!-and-make-specialized-compilers
                    keywords fn-type fn-type-cctor compilers)]
               (old-fn-compiler ast specialized-compilers))
             (old-fn-compiler ast compilers)))))))
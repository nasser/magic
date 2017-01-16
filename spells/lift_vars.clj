(ns magic.spells.lift-vars
  (:require [magic.core :as magic]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [System.Reflection FieldAttributes BindingFlags MethodAttributes]))

(defn var-name [v]
  (.Replace
    (str (.Namespace v) "_" (.Symbol v))
    "."
    "_"))

(defn private-constructor [t]
  (first (.GetConstructors t (enum-or BindingFlags/NonPublic BindingFlags/Instance))))

(def default-constructor
  (il/constructor
    (enum-or MethodAttributes/Public)
    CallingConventions/Standard
    []
    [(il/ldarg-0)
     (il/call (private-constructor clojure.lang.AFunction))
     (il/ret)]))

(def field-attrs
  (enum-or FieldAttributes/InitOnly
           FieldAttributes/Private
           FieldAttributes/Static))

(defn var-field-map [vars]
  (apply hash-map
         (interleave
           (->> vars (map :var))
           (->> vars (map :var)
                (map #(il/field clojure.lang.Var
                                (var-name %)
                                field-attrs))))))

(defn lift-vars [symbolizers]
  (update symbolizers
          :fn
          (fn 
            [old-fn-symbolizer]
            (fn lifted-var-fn-symbolizer
              [{:keys [vars] :as ast} symbolizers]
              (let [vars (set vars)
                    var-map (var-field-map vars)
                    specialized-symbolizers
                    (assoc
                      symbolizers
                      :var
                      (fn lifted-var-symbolizer
                        [{:keys [var] :as ast} symbolizers]
                        [(il/ldsfld (var-map var))
                         (magic/get-var var)
                         (magic/cleanup-stack ast)]))]
                (-> ast
                    (old-fn-symbolizer specialized-symbolizers)
                    (update ::il/body concat
                            [default-constructor
                             (il/constructor
                               (enum-or MethodAttributes/Public MethodAttributes/Static)
                               CallingConventions/Standard
                               []
                               [(interleave
                                  (->> vars (map :var) (map magic.core/load-var))
                                  (->> vars (map :var) (map #(il/stsfld (var-map %)))))
                                (il/ret)])])))))))
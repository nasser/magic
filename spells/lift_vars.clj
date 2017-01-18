(ns magic.spells.lift-vars
  (:require [magic.core :as magic]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [System.Reflection FieldAttributes BindingFlags MethodAttributes]))

(defn var-name [v]
  (.Replace
    (str (.Namespace v) "/" (.Symbol v))
    "."
    "$"))

(def field-attrs
  (enum-or FieldAttributes/InitOnly
           FieldAttributes/Private
           FieldAttributes/Static))

(defn var-field-map [vars]
  (apply hash-map
         (interleave
           (->> vars (map :var))
           (->> vars (map :var)
                (map #(il/field (if (-> % meta :static)
                                  Object ; (-> % deref type)
                                  clojure.lang.Var)
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
                        (let [{:keys [static]} (meta var)]
                          [(il/ldsfld (var-map var))
                           (when-not static
                             (magic/get-var var))
                           (magic/cleanup-stack ast)])))]
                (-> ast
                    (old-fn-symbolizer specialized-symbolizers)
                    (update ::il/body concat
                            [(il/constructor
                               (enum-or MethodAttributes/Public MethodAttributes/Static)
                               CallingConventions/Standard
                               []
                               [(interleave
                                  (->> vars (map :var) (map magic/load-var))
                                  (->> vars (map :var) (map #(if (-> % meta :static)
                                                               (magic/get-var %))))
                                  (->> vars (map :var) (map #(il/stsfld (var-map %)))))
                                (il/ret)])])))))))
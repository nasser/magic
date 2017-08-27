(ns magic.spells.lift-vars
  (:require [magic.core :as magic]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [System.Reflection CallingConventions FieldAttributes BindingFlags MethodAttributes]))

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

(defn lift-vars [compilers]
  (update compilers
          :fn
          (fn 
            [old-fn-compiler]
            (fn lifted-var-fn-compiler
              [{:keys [vars] :as ast} compilers]
              (let [vars (set vars)
                    var-map (var-field-map vars)
                    specialized-compilers
                    (assoc
                      compilers
                      :var
                      (fn lifted-var-compiler
                        [{:keys [var] :as ast} compilers]
                        (let [{:keys [static]} (meta var)]
                          [(il/ldsfld (var-map var))
                           (when-not static
                             (magic/get-var var))
                           (magic/cleanup-stack ast)])))]
                (-> ast
                    (old-fn-compiler specialized-compilers)
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
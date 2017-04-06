(ns magic.spells.protocols
  (:require [magic.core :as magic]
            [magic.analyzer.types :refer [clr-type]]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [System.Reflection FieldAttributes]))

(def cached-field-attrs
  (enum-or FieldAttributes/Private FieldAttributes/InitOnly FieldAttributes/Static))

(defn protocols [cached-type?]
  (fn [symbolizers]
  (update symbolizers
          :invoke
          (fn [old-invoke-symbolizer]
            (fn protocol-invoke-symbolizer
              [{:keys [fn args] :as ast} symbolizers]
              (if-let [proto (-> fn :meta :protocol)]
                (let [iface (-> proto deref :on-interface)
                      target (first args)
                      fn-args (rest args)
                      fn-name (-> fn :var .Symbol str)
                      method (apply interop/method
                                    iface
                                    fn-name
                                    (repeat (count fn-args) Object))
                      fn-il (magic/symbolize fn symbolizers)
                      target-il (magic/symbolize (first args) symbolizers)
                      cached-type (il/field Type
                                            (str (gensym (str fn-name "-cached-type" )))
                                            cached-field-attrs)
                      cache-hit (il/label)
                      not-iface (il/label)
                      end (il/label)]
                  [(when cached-type?
                     [target-il
                      (il/callvirt (interop/method Object "GetType"))
                      (il/ldsfld cached-type)
                      (il/ceq)
                      (il/brtrue cache-hit)])
                     target-il
                     (il/isinst iface)
                     (il/ldnull)
                     (il/cgt-un)
                     (il/brfalse not-iface)
                       target-il
                       (il/castclass iface)
                       (interleave
                         (map magic/symbolize fn-args)
                         (map magic/convert
                              (map clr-type fn-args)
                              (interop/parameter-types method)))
                       (il/callvirt method)
                       (il/br end)
                     not-iface
                     (when cached-type?
                       [target-il
                        (il/callvirt (interop/method Object "GetType"))
                        (il/stsfld cached-type)
                        cache-hit])
                   (old-invoke-symbolizer ast symbolizers)
                   end])
                (old-invoke-symbolizer ast symbolizers)))))))

(def protocols-type-cache (protocols true))
(def protocols-no-type-cache (protocols false))
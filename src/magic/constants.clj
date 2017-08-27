(ns magic.constants
  (:require [mage.core :as il])
  (:import 
    [System.Reflection
     CallingConventions
            MethodAttributes
            FieldAttributes]))

(def public-static-const
  (enum-or FieldAttributes/Static FieldAttributes/InitOnly FieldAttributes/Public))

(defn emit! []
  (il/emit!
    (il/assembly+module
      "Constants"
      (il/type
        "Magic.Constants"
        (let [t (il/field Object "True" public-static-const)
              f (il/field Object "False" public-static-const)]
          (il/constructor
            (enum-or MethodAttributes/Public MethodAttributes/Static)
            CallingConventions/Standard
            []
            [(il/ldc-i4-1)
             (il/box Boolean)
             (il/stsfld t)
             (il/ldc-i4-0)
             (il/box Boolean)
             (il/stsfld f)
             (il/ret)]))))))
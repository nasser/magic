;; a place for new ideas
(ns magic.analyzer.novel
  (:require [magic.analyzer.analyze-host-forms :as host]
            [magic.analyzer.types :refer [read-generic-name]]
            [clojure.string :as string]))

(def operators
  '{++ op_Increment
    -- op_Decrement
    ; -  op_UnaryNegation
    ; +  op_UnaryPlus
    !  op_LogicalNot
    |~|  op_OnesComplement
    /  op_Division
    %  op_Modulus
    *  op_Multiply
    +  op_Addition
    -  op_Subtraction
    << op_LeftShift
    >> op_RightShift
    >  op_GreaterThan
    <  op_LessThan
    >= op_GreaterThanOrEqual
    <= op_LessThanOrEqual
    == op_Equality
    != op_Inequality
    &  op_BitwiseAnd
    |^|  op_ExclusiveOr
    ; |  op_BitwiseOr
    })

(defn csharp-operators
  "Analyze (Foo/+ bar) into (Foo/op_Addition bar)"
  {:pass-info {:walk :post
               :depends #{}
               :before #{#'host/analyze-byref #'host/analyze-type
                         #'host/analyze-host-field #'host/analyze-constructor
                         #'host/analyze-host-interop #'host/analyze-host-call}}}
  [{:keys [method target args op] :as ast}]
  (if (= :host-call op)
    (if-let [operator-method (operators method)]
      (assoc ast :method operator-method
        :novel true)
      ast)
    ast))

(defn generic-type-syntax 
  "Analyze Foo|[String, Int32]| into Foo`2[String, Int32]"
  {:pass-info {:walk :post
               :depends #{}
               :before #{#'host/analyze-byref #'host/analyze-type
                         #'host/analyze-host-field #'host/analyze-constructor
                         #'host/analyze-host-interop #'host/analyze-host-call}}}
  [{:keys [op children class] :as ast}]
  (if (= :maybe-class op)
    (if (re-find #"\[" (str class))
      (let [[class-name type-args] (read-generic-name class)
            type-args-str (str "[" (string/join "," type-args) "]")
            type-name (if (empty? type-args)
                        (str class-name type-args)
                        (str class-name "`" (count type-args) type-args-str))]
        (merge ast {:class (symbol type-name)}))
      ast)
    ast))
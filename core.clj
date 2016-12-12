(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [clojure.tools.analyzer.clr :as ana]
            [clojure.tools.analyzer.clr.types :refer [clr-type best-match]]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang Var RT IFn]
           [System.IO FileInfo Path]
           [System.Reflection.Emit OpCodes]
           [System.Reflection
            TypeAttributes
            MethodAttributes
            FieldAttributes
            FieldInfo
            MethodInfo
            PropertyInfo]
           System.AppDomain))

(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))

(defn load-argument [i]
  (cond
    (= i 0) (il/ldarg-0)
    (= i 1) (il/ldarg-1)
    (= i 2) (il/ldarg-2)
    (= i 3) (il/ldarg-3)
    :else (il/ldarg i)))

(defmulti load-constant type)

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " (type k)))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (il/ldc-i4 k))

(defmethod load-constant Int64 [k]
  (il/ldc-i8 k))

(defmethod load-constant Single [k]
  (il/ldc-r4 k))

(defmethod load-constant Double [k]
  (il/ldc-r8 k))

(defmethod load-constant Boolean [k]
  (if k (il/ldc-i4-1) (il/ldc-i4-0)))

;; NOTE the stock compiler looks up types using RT.classForName
;; if the type is not a valuetype. why? does it make a difference?
(defmethod load-constant Type [v]
  [(il/ldtoken v)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])


(defn load-var [v]
  (let [nsname  (.. v Namespace Name ToString)
        symname (.. v Symbol ToString)]
    [(load-constant nsname)
     (load-constant symname)
     (il/call (interop/method RT "var" String String))]))

(defn get-var [v]
  (if (.isDynamic v)
    (il/call (interop/method Var "get"))
    (il/call (interop/method Var "getRawRoot"))))

;; TODO overflows?
;; can overflow opcodes replace e.g. RT.intCast?
(def intrinsic-conv
  {Char   (il/conv-u2)
   SByte  (il/conv-i1)
   Byte   (il/conv-u1)
   Int16  (il/conv-i2)
   Int32  (il/conv-i4)
   Int64  (il/conv-i8)
   Double (il/conv-r8)
   Single (il/conv-r4)
   UInt16 (il/conv-u2)
   UInt32 (il/conv-u4)
   UInt64 (il/conv-u8)})

(defn convert [from to]
  (swap! debug (str "Converting " from " -> " to))
  (cond
    (nil? from)
    nil
    
    ;; do nothing if the types are the same 
    (= from to)
    nil
    
    ;; cannot convert nil to value type
    (and (nil? from) (.IsValueType to))
    (throw (Exception. (str "Cannot convert nil to value type " to)))
        
    ;; TODO truthiness
    (= to Boolean)
    (let [isbool (il/label)
          fls (il/label)
          end (il/label)]
      [(il/dup)
       (il/isinst Boolean)
       (il/brtrue isbool)
       (il/ldnull)
       (il/ceq)
       (il/brtrue fls)
       (il/ldc-i4-1)
       (il/br end)
       fls
       (il/ldc-i4-0)
       (il/br end)
       isbool
       (il/unbox-any Boolean)
       end])
    
    ;; convert void to nil
    ;; TODO is this a terrible idea?
    (and (= System.Void from) (not (.IsValueType to)))
    (il/ldnull)
    
    (and (= System.Void from) (.IsValueType to))
    (throw (Exception. (str "Cannot convert void to value type " to)))
    
    ;; use user defined implicit conversion if it exists
    (interop/method from "op_Implicit" to)
    (il/call (interop/method from "op_Implicit" to))
    
    ;; use user defined explicit conversion if it exists
    (interop/method from "op_Explicit" to)
    (il/call (interop/method from "op_Explicit" to))
    
    ;; use intrinsic conv opcodes from primitive to primitive
    (and (.IsPrimitive from) (.IsPrimitive to))
    (intrinsic-conv to)
    
    ;; box valuetypes to objects
    (and (.IsValueType from) (= to Object))
    (il/box from)
    
    ;; RT casts
    (and (= from Object) (= to Single))
    (il/call (interop/method RT "floatCast" from))
    (and (= from Object) (= to Double))
    (il/call (interop/method RT "doubleCast" from))
    (and (= from Object) (= to Int32))
    (il/call (interop/method RT "intCast" from))
    (and (= from Object) (= to Int64))
    (il/call (interop/method RT "longCast" from))
    
    ;; unbox objects to valuetypes
    (and (= from Object) (.IsValueType to))
    (il/unbox-any to)
    
    ;; castclass if to is a subclass of from
    (.IsSubclassOf to from)
    (il/castclass to)
    
    ;; do nothing if converting to super class
    (.IsSubclassOf from to)
    nil
    
    :else
    (throw (Exception. (str "Cannot convert " from " to " to)))))

(defn cleanup-stack
  "il/pop if in a non-void statement context.
  Required to keep the stack balanced."
  ([{{:keys [context]} :env}]
   (if (= context :ctx/statement)
     (il/pop)))
  ([lasttype {{:keys [context]} :env}]
   (if-not (= lasttype System.Void)
     (cleanup-stack context))))

;;; symbolizers
(def symbolize)

(defn do-symbolizer
  [{:keys [statements ret]} symbolizers]
  [(map #(symbolize % symbolizers) statements)
   (symbolize ret symbolizers)])

(defn const-symbolizer
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast}
   symbolizers]
  [(load-constant val)
   (cleanup-stack ast)])


(defn static-property-symbolizer
  "Symbolic bytecode for static properties"
  [{:keys [property]} symbolizers]
  (il/call (.GetGetMethod property)))

;; TODO keep an eye on this
(defn reference-to [ast]
  (when (.IsValueType (clr-type ast))
    (let [local (il/local (clr-type ast) ast)]
      [(il/stloc local)
       (il/ldloca local)
       ])))


(defn instance-property-symbolizer
  "Symbolic bytecode for instance properties"
  [{:keys [target property]} symbolizers]
  [(symbolize target symbolizers)
   (reference-to target)
   (il/call (.GetGetMethod property))])


(defn static-field-symbolizer
  "Symbolic bytecode for static fields"
  [{:keys [field]} symbolizers]
  (if (.IsLiteral field)
    (load-constant (.GetRawConstantValue field))
    (il/ldsfld field)))


(defn instance-field-symbolizer
  "Symbolic bytecode for instance fields"
  [{:keys [field target]} symbolizers]
  [(symbolize target symbolizers)
   (reference-to target)
   (il/ldfld field)])


(defn inexact-static-method-symbolizer
  "Symbolic bytecode for static methods"
  [{:keys [method target args methods] :as ast} symbolizers]
  (let [arg-types (map clr-type args)]
    (if-let [matching-method (best-match ast :methods)]
      [(interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types matching-method)))
       (il/call matching-method)]
      (throw! "No static method " method
              " on type " (:val target)
              " matching signature " (vec arg-types)))))

(defn static-method-symbolizer
  ;; TODO inexact-static-method should be its own :op
  "Symbolic bytecode for static methods"
  [{:keys [method args inexact?] :as ast} symbolizers]
  (if inexact?
    (inexact-static-method-symbolizer
      ast symbolizers)
    (let [arg-types (map clr-type args)]
      [(interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types method)))
       (il/call method)])))


(defn inexact-instance-method-symbolizer
  [{:keys [method target args inexact? generic-parameters] :as ast} symbolizers]
  (let [arg-types (map clr-type args)]
    (if-let [matching-method (best-match ast :methods)]
      [(symbolize target symbolizers)
       (interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types matching-method)))
       (cond
         (and (.IsVirtual matching-method)
              (nil? generic-parameters))
         (il/callvirt matching-method)
         
         (and (not (.IsVirtual matching-method))
              (nil? generic-parameters))
         (il/call matching-method)
         
         (and (.IsVirtual matching-method)
              generic-parameters)
         (il/emit-callvirt matching-method generic-parameters)
         
         (and (not (.IsVirtual matching-method))
              generic-parameters)
         (il/emit-call matching-method generic-parameters))]
      (throw! "No static method " method
              " on type " (clr-type target)
              " matching signature " (vec arg-types)))))

(defn instance-method-symbolizer
  ;; TODO inexact-instance-method should be its own :op
  "Symbolic bytecode for instance methods"
  [{:keys [method target args inexact? generic-parameters] :as ast} symbolizers]
  (if inexact?
    (inexact-instance-method-symbolizer ast symbolizers)
    (let [arg-types (map clr-type args)]
      [(symbolize target symbolizers)
       (interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types method)))
       (cond
         (and (.IsVirtual method)
              (nil? generic-parameters))
         (il/callvirt method)
         
         (and (not (.IsVirtual method))
              (nil? generic-parameters))
         (il/call method)
         
         (and (.IsVirtual method)
              generic-parameters)
         (il/emit-callvirt method generic-parameters)
         
         (and (not (.IsVirtual method))
              generic-parameters)
         (il/emit-call method generic-parameters))])))

(defn initobj-symbolizer
  "Symbolic bytecode for zero arity value type constructor invocation"
  [{:keys [type args]} symbolizers]
  (let [loc (il/local type)]
    [(il/ldloca-s loc)
     (il/initobj type)
     (il/ldloc loc)]))

(defn inexact-new-symbolizer
  [{:keys [inexact? type args] :as ast} symbolizers]
  (let [arg-types (map clr-type args)]
    (if-let [matching-constructor (best-match ast :constructors)]
      [(interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types matching-constructor)))
       (il/newobj matching-constructor)]
      (throw! "No constructor for type " type
              " matching signature " (vec arg-types)))))

(defn new-symbolizer
  "Symbolic bytecode for constructor invocation"
  [{:keys [inexact? type constructor args] :as ast} symbolizers]
  (if inexact?
    (inexact-new-symbolizer ast symbolizers)
    (let [arg-types (map clr-type args)]
      [(interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types constructor)))
       (il/newobj constructor)])))

(defn let-symbolizer
  [{:keys [bindings body] :as ast} symbolizers]
  (let [;; uniqued names -> il/locals
        binding-map (reduce (fn [m binding]
                              (assoc m
                                (-> binding :name) 
                                (il/local (clr-type binding))))
                            {} bindings)
        recur-target (il/label)
        specialized-symbolizers
        (assoc symbolizers
          :local
          (fn let-local-symbolizer
            [{:keys [name] {:keys [locals]} :env :as ast} syms]
            (if-let [loc (-> name binding-map)]
              (il/ldloc loc)
              (symbolize ast symbolizers)))
          )]
    
    ;; emit local initializations
    [(map (fn [binding]
            [(symbolize binding specialized-symbolizers)
             (il/stloc (binding-map (:name binding)))])
          bindings)
     
     ;; mark recur target
     recur-target
     
     ;; emit body with specialized symbolizers
     (symbolize body specialized-symbolizers)
     (cleanup-stack ast)]))

(defn binding-symbolizer
  [{:keys [init] :as ast} symbolizers]
  [(symbolize init symbolizers)
   (cleanup-stack ast)])

(defn local-symbolizer
  [{:keys [name arg-id local] :as ast} symbolizers]
  (if (= local :arg)
    ;; TODO only inc arg-id in instance methods, how do we know?
    (load-argument (inc arg-id))
    (throw! "Local " name " not an argument and could not be symbolized")))

(defn invoke-symbolizer
  [{:keys [fn args] :as ast} symbolizers]
  [(symbolize fn symbolizers)
   (il/castclass IFn)
   (interleave
     (map #(symbolize % symbolizers) args)
     (map #(convert (clr-type %) Object) args))
   (il/callvirt (apply interop/method IFn "invoke" (repeat (count args) Object)))
   (cleanup-stack ast)])

(defn var-symbolizer
  [{:keys [var] :as ast} symbolizers]
  [(load-var var)
   (get-var var)
   (cleanup-stack ast)])

(defn has-arity-method
  "Symbolic bytecode for the IFnArity.HasArity method"
  [arities]
  (il/method
    "HasArity"
    (enum-or MethodAttributes/Public
             MethodAttributes/Virtual)
    Boolean [Int32]
    (let [ret-true (il/label)]
      [(map (fn [arity]
              [(il/ldarg-1)
               (load-constant arity)
               (il/beq ret-true)])
            arities)
       (il/ldc-i4-0)
       (il/ret)
       ret-true
       (il/ldc-i4-1)
       (il/ret)])))

(defn fn-symbolizer
  [{:keys [methods raw-forms] :as ast} symbolizers]
  (let [name (str (gensym "fn"))
        arities (map :fixed-arity methods)]
    (mage.core/type
      name
      TypeAttributes/Public []
      clojure.lang.AFn
      [(has-arity-method arities)
       (map #(symbolize % symbolizers) methods)])))

(defn fn-method-symbolizer
  [{:keys [body params] {:keys [ret statements]} :body} symbolizers]
  (il/method "invoke"
             (enum-or MethodAttributes/Public
                      MethodAttributes/Virtual)
             ;; Object (mapv (constantly Int64) params)
             (clr-type ret) (mapv clr-type params)
             [(symbolize body symbolizers)
              (convert (clr-type ret) Object)
              (il/ret)]))

(def base-symbolizers
  {:const               #'const-symbolizer
   :do                  #'do-symbolizer
   :fn                  #'fn-symbolizer
   :let                 #'let-symbolizer
   :local               #'local-symbolizer
   :binding             #'binding-symbolizer
   :invoke              #'invoke-symbolizer
   :var                 #'var-symbolizer
   :the-var             #'var-symbolizer
   :fn-method           #'fn-method-symbolizer
   :static-property     #'static-property-symbolizer
   :instance-property   #'instance-property-symbolizer
   :static-field        #'static-field-symbolizer
   :instance-field      #'instance-field-symbolizer
   :static-method       #'static-method-symbolizer
   :instance-method     #'instance-method-symbolizer
   :initobj             #'initobj-symbolizer
   :new                 #'new-symbolizer
   })

(defn ast->symbolizer
  "Look up symbolizer for AST node. Throws exception if not found."
  [ast symbolizers]
  (or (-> ast :op symbolizers)
      (throw (Exception. (str "No symbolizer for " (pr-str (or  (:op ast)
                                                               ast)))))))

(defn symbolize
  "Generate symbolic bytecode for AST node"
  [ast symbolizers]
  (if-let [symbolizer (ast->symbolizer ast symbolizers)]
    (symbolizer ast symbolizers)))

(defn compile-fn
  "Compile fn form using base-symbolizers, emit binary to current directory, return fn"
  ([expr] (compile-fn expr "magic.compile"))
  ([expr asm-name]
   (->> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (symbolize (ana/analyze expr) base-symbolizers)))
        flatten
        (remove nil?)
        il/emit!
        :mage.core/assembly-builder
        .GetTypes
        first
        Activator/CreateInstance
        )))

(defn compile-il
  "Compile bytes"
  ([il] (compile-il il "magic.compile"))
  ([il asm-name]
   (-> (il/assembly
         asm-name
         (il/module
           (str asm-name ".dll")
           (mage.core/type
             (str (gensym "RawBytecode"))
             TypeAttributes/Public []
             Object
             [(il/method "invoke"
                         (enum-or MethodAttributes/Public
                                  MethodAttributes/Virtual)
                         Object []
                         il)])))
       il/emit!
       :mage.core/assembly-builder
       .GetTypes
       first
       Activator/CreateInstance
       )))
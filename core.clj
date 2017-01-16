(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [clojure.tools.analyzer.clr :as ana]
            [clojure.tools.analyzer.clr.util :refer [var-interfaces]]
            [clojure.tools.analyzer.clr.types :refer [clr-type non-void-clr-type best-match]]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang Var RT IFn Keyword]
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
    (< i 16) (il/ldarg-s i) ;; TODO what is the cutoff?
    :else (il/ldarg-s i)))

(defmulti load-constant type)

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " (type k)))

(defmethod load-constant nil [k]
  (il/ldnull))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (il/ldc-i4 k))

(defmethod load-constant Int64 [k]
  (il/ldc-i8 k))

;; TODO BigInt vs BigInteger?
(defmethod load-constant clojure.lang.BigInt [k]
  (if (nil? (.Bipart k))
    [(load-constant (.Lpart k))
     (il/call (interop/method clojure.lang.BigInt "fromLong" Int64))]
    [(load-constant (str k "N"))
     (il/call (interop/method clojure.lang.RT "readString" String))]))

(defmethod load-constant Single [k]
  (il/ldc-r4 k))

(defmethod load-constant Double [k]
  (il/ldc-r8 k))

(defmethod load-constant Boolean [k]
  (if k (il/ldc-i4-1) (il/ldc-i4-0)))

(defmethod load-constant Keyword [k]
  [(load-constant (.Namespace k))
   (load-constant (.Name k))
   (il/call (interop/method Keyword "intern" String String))])

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
    (and (.IsValueType from)
         (= to Boolean))
    [(il/pop)
     (il/ldc-i4-1)]
    
    (and
      (= from Object)
      (= to Boolean))
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
       (il/br-s end)
       fls
       (il/ldc-i4-0)
       (il/br-s end)
       isbool
       (il/unbox-any Boolean)
       end])
    
    (= to Boolean)
    [(il/ldnull)
     (il/ceq)]
    

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
    #_
    (let [fail (il/label)
          end (il/label)]
      [(il/dup)
       (il/isinst to)
       (il/brfalse fail)
       (il/unbox-any to)
       (il/br end)
       fail
       (il/callvirt (interop/method Object "GetType"))
       (il/callvirt (interop/method Type "get_FullName"))
       (il/ldstr (str " to " to))
       (il/call (interop/method String "Concat" String String))
       (il/newobj (interop/constructor InvalidCastException String))
       (il/throw)
       end])
    (il/unbox-any to)

    ;; castclass if to is a subclass of from
    (.IsSubclassOf to from)
    (il/castclass to)

    ;; do nothing if converting to super class
    (.IsSubclassOf from to)
    nil

    :else
    (throw (Exception. (str "Cannot convert " from " to " to)))))

(defn statement? [{{:keys [context]} :env}]
  (= context :ctx/statement))

(defn cleanup-stack
  "il/pop if in a non-void statement context.
  Required to keep the stack balanced."
  [ast]
  (if (and (statement? ast)
           (not= System.Void (clr-type ast)))
    (il/pop)))

;;; symbolizers
(def symbolize)

(defn do-symbolizer
  [{:keys [statements ret]} symbolizers]
  [(interleave
     (map #(symbolize % symbolizers) statements)
     (map #(cleanup-stack %) statements))
   (symbolize ret symbolizers)])

(defn const-symbolizer
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast} symbolizers]
  (load-constant val))

(defn prepare-array [items symbolizers]
  [(il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (symbolize c symbolizers)
           (convert (clr-type c) Object)
           (il/stelem-ref)])
        (range)
        items)])

(defn vector-symbolizer
  [{:keys [items]} symbolizers]
  [(load-constant (int (count items)))
   (prepare-array items symbolizers)
   (il/call (interop/method clojure.lang.RT "vector" |System.Object[]|))])

(defn set-symbolizer
  [{:keys [items]} symbolizers]
  [(load-constant (int (count items)))
   (prepare-array items symbolizers)
   (il/call (interop/method clojure.lang.RT "set" |System.Object[]|))])

(defn map-symbolizer
  [{:keys [keys vals]} symbolizers]
  [(load-constant (int (+ (count keys) (count vals))))
   (prepare-array (interleave keys vals) symbolizers)
   (il/call (interop/method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))])

(defn static-property-symbolizer
  "Symbolic bytecode for static properties"
  [{:keys [property]} symbolizers]
  (il/call (.GetGetMethod property)))

;; TODO keep an eye on this
;; TODO il/ldarga, il/ldarga-s for references to args
(defn reference-to [{:keys [local arg-id] :as ast}]
  (when (.IsValueType (clr-type ast))
    (let [local (il/local (clr-type ast) ast)]
      [(il/stloc local)
       (il/ldloca local)])))


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

(defn dynamic-field-symbolizer
  "Symbolic bytecode for dynamic fields"
  [{:keys [field target]} symbolizers]
  (let [target-local (il/local)
        target-type (il/local Type)
        reflected-field (il/local FieldInfo)
        valid-field (il/label)]
    [(symbolize target symbolizers)
     (reference-to target)
     (il/stloc target-local)
     (il/ldloc target-local)
     (il/callvirt (interop/method Object "GetType"))
     (il/stloc target-type)
     (il/ldloc target-type)
     (load-constant (str field))
     (il/callvirt (interop/method Type "GetField" String))
     (il/stloc reflected-field)
     (il/ldloc reflected-field)
     (il/brtrue valid-field)
     (il/ldnull)
     valid-field
     (il/ldloc reflected-field)
     (il/ldloc target-local)
     (il/callvirt (interop/method FieldInfo "GetValue" Object))
     ]))

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
         ;; TODO ???
         (and (.IsVirtual matching-method)
              generic-parameters)
         (il/callvirt matching-method generic-parameters)
         ;; TODO ???
         (and (not (.IsVirtual matching-method))
              generic-parameters)
         (il/call matching-method generic-parameters))]
      (throw! "No static method " method
              " on type " (clr-type target)
              " matching signature " (vec arg-types)))))

(defn instance-method-symbolizer
  ;; TODO inexact-instance-method should be its own :op
  "Symbolic bytecode for instance methods"
  [{:keys [method target args inexact? generic-parameters] :as ast} symbolizers]
  (if inexact?
    (inexact-instance-method-symbolizer ast symbolizers)
    (let [arg-types (map clr-type args)
          virtcall (if (.IsValueType (clr-type target))
                     il/call
                     il/callvirt )]
      [(symbolize target symbolizers)
       ; (reference-to target)
       (interleave
         (map #(symbolize % symbolizers) args)
         (map convert
              arg-types
              (interop/parameter-types method)))
       (cond
         (and (.IsVirtual method)
              (nil? generic-parameters))
         [(reference-to target)
          (virtcall method)]

         (and (not (.IsVirtual method))
              (nil? generic-parameters))
         (il/call method)
         ;; TODO ???
         (and (.IsVirtual method)
              generic-parameters)
         [(reference-to target)
          (virtcall method generic-parameters)]
         ;; TODO ???
         (and (not (.IsVirtual method))
              generic-parameters)
         (il/call method generic-parameters))])))

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
  [{:keys [bindings body loop-id] :as ast} symbolizers]
  (let [;; uniqued names -> il/locals
        binding-map (reduce (fn [m binding]
                              (assoc m
                                (-> binding :name)
                                (il/local (clr-type binding))))
                            (sorted-map) 
                            bindings)
        binding-vector (mapv #(binding-map (-> % :name)) bindings)
        recur-target (il/label)
        ;; TODO symbolizer local and recur with symbolizers or syms?
        specialized-symbolizers
        (merge symbolizers
               {:local
                (fn let-local-symbolizer
                  [{:keys [name] {:keys [locals]} :env :as ast} syms]
                  (if-let [loc (-> name binding-map)]
                    (il/ldloc loc)
                    (symbolize ast symbolizers)))}
               (when loop-id
                 {:recur
                  (fn let-recur-symbolizer
                    [{:keys [exprs] :as ast} syms]
                    (let [expr-range (range (count exprs))
                          temporaries
                          (mapv #(il/local (::il/type (nth binding-vector %)))
                                expr-range)]
                      [(interleave
                         (map #(symbolize % syms) exprs)
                         (map #(convert (clr-type %1)
                                        (::il/type (temporaries %2)))
                              exprs
                              expr-range)
                         (map #(il/stloc (temporaries %)) expr-range))
                       (interleave
                         (map #(il/ldloc (temporaries %)) expr-range)
                         (map #(il/stloc (nth binding-vector %)) expr-range))
                       (il/br recur-target)]))}))]
    ;; emit local initializations
    [(map (fn [binding]
            [(symbolize binding specialized-symbolizers)
             (convert (clr-type (-> binding :init)) (clr-type binding))
             (il/stloc (binding-map (:name binding)))])
          bindings)
     ;; mark recur target
     (when loop-id
       recur-target)
     ;; emit body with specialized symbolizers
     (symbolize body specialized-symbolizers)]))

(defn if-symbolizer
  [{:keys [test then else] :as ast} symbolizers]
  (let [if-expr-type (clr-type ast)
        then-label (il/label)
        end-label (il/label)]
    [(symbolize test symbolizers)
     (convert (clr-type test) Boolean)
     (il/brtrue then-label)
     (symbolize else symbolizers)
     (when-not (= :recur (:op else))
       (convert (clr-type else) if-expr-type))
     (il/br end-label)
     then-label
     (symbolize then symbolizers)
     (when-not (= :recur (:op then))
       (convert (clr-type then) if-expr-type))
     end-label]))

(defn binding-symbolizer
  [{:keys [init] :as ast} symbolizers]
  (symbolize init symbolizers))

(defn local-symbolizer
  [{:keys [name arg-id local] :as ast} symbolizers]
  (if (= local :arg)
    ;; TODO only inc arg-id in instance methods, how do we know?
    (load-argument (inc arg-id))
    (throw! "Local " name " not an argument and could not be symbolized")))

(defn invoke-symbolizer
  [{:keys [fn args] :as ast} symbolizers]
  (let [arg-types (map clr-type args)
        target-interfaces (var-interfaces fn)
        exact-match (->> target-interfaces
                         (filter #(= (drop 1 (.GetGenericArguments %))
                                     arg-types))
                         first)]
    [(symbolize fn symbolizers)
     (if exact-match
       [(il/castclass exact-match)
        (interleave
          (map #(symbolize % symbolizers) args)
          (map #(convert (clr-type %1) %2) args arg-types))
        (il/callvirt (apply interop/method exact-match "invoke" arg-types))]
       [(il/castclass IFn)
        (interleave
          (map #(symbolize % symbolizers) args)
          (map #(convert (clr-type %) Object) args))
        (il/callvirt (apply interop/method IFn "invoke" (repeat (count args) Object)))])]))

(defn var-symbolizer
  [{:keys [var] :as ast} symbolizers]
  [(load-var var)
   (get-var var)])

(defn set!-symbolizer
  [{:keys [target val] {:keys [context]} :env :as ast} symbolizers]
  (let [target-op (:op target)
        target' (-> target :target)
        field (-> target :field)
        property (-> target :property)]
    (cond
      (= target-op :instance-field)
      (let [v (il/local (clr-type val))]
        [(symbolize target' symbolizers)
         (reference-to target')
         (symbolize val symbolizers)
         (il/stloc v)
         (il/ldloc v)
         (convert (clr-type val) (.FieldType field))
         (il/stfld field)
         (il/ldloc v)])
      (= target-op :instance-property)
      (let [v (il/local (clr-type val))]
        [(symbolize target' symbolizers)
         (reference-to target')
         (symbolize val symbolizers)
         (il/stloc v)
         (il/ldloc v)
         (convert (clr-type val) (.PropertyType property))
         (il/callvirt (.GetSetMethod property))
         (il/ldloc v)])
      (= target-op :static-field)
      (throw! "set! static-field not finished")
      (= target-op :static-property)
      (throw! "set! static-property not finished"))))

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
  [{:keys [local methods raw-forms] :as ast} symbolizers]
  (let [name (str (gensym
                    (str (or (-> local :form)
                             "magic_fn_")
                         "$")))
        arities (map :fixed-arity methods)
        param-types (->> methods
                         (map :params)
                         (mapcat #(vector (map non-void-clr-type %)
                                          (map (constantly Object) %))))
        return-types (->> methods
                          (mapcat #(vector
                                     (or (-> % :form first meta :tag)
                                         (-> % :body non-void-clr-type))
                                     Object)))
        interfaces (map #(interop/generic-type "Function" (conj %1 %2))
                        param-types
                        return-types)]
    (il/type
      name
      TypeAttributes/Public
      interfaces
      clojure.lang.AFunction
      [(has-arity-method arities)
       (map #(symbolize % symbolizers) methods)])))

(defn fn-method-symbolizer
  [{:keys [body params] {:keys [ret statements]} :body} symbolizers]
  (let [param-types (mapv clr-type params)
        obj-params (mapv (constantly Object) params)
        return-type (non-void-clr-type ret)
        public-virtual (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        unhinted-method
        (il/method
          "invoke"
          public-virtual
          Object obj-params
          [(symbolize body symbolizers)
           (convert return-type Object)
           (il/ret)])
        hinted-method
        (il/method
          "invoke"
          public-virtual
          return-type param-types
          [(symbolize body symbolizers)
           (convert (clr-type ret) return-type)
           (il/ret)])
        unhinted-shim
        (il/method
          "invoke"
          public-virtual
          Object obj-params
          [(il/ldarg-0)
           (interleave
             (map (comp load-argument inc) (range))
             (map #(convert Object %) param-types))
           (il/callvirt hinted-method)
           (convert return-type Object)
           (il/ret)])]
    [(if (and (= param-types obj-params)
              (= return-type Object))
       unhinted-method
       [hinted-method unhinted-shim])]))

(def base-symbolizers
  {:const               #'const-symbolizer
   :do                  #'do-symbolizer
   :vector              #'vector-symbolizer
   :set                 #'set-symbolizer
   :map                 #'map-symbolizer
   :fn                  #'fn-symbolizer
   :if                  #'if-symbolizer
   :let                 #'let-symbolizer
   :loop                #'let-symbolizer
   :local               #'local-symbolizer
   :binding             #'binding-symbolizer
   :invoke              #'invoke-symbolizer
   :var                 #'var-symbolizer
   :the-var             #'var-symbolizer
   :set!                #'set!-symbolizer
   :fn-method           #'fn-method-symbolizer
   :static-property     #'static-property-symbolizer
   :instance-property   #'instance-property-symbolizer
   :static-field        #'static-field-symbolizer
   :instance-field      #'instance-field-symbolizer
   :dynamic-field       #'dynamic-field-symbolizer
   :static-method       #'static-method-symbolizer
   :instance-method     #'instance-method-symbolizer
   :initobj             #'initobj-symbolizer
   :new                 #'new-symbolizer
   })

(def ^:dynamic *initial-symbolizers* nil)

(defn ast->symbolizer
  "Look up symbolizer for AST node. Throws exception if not found."
  [ast symbolizers]
  (or (-> ast :op symbolizers)
      (throw (Exception. (str "No symbolizer for " (pr-str (or  (:op ast)
                                                                ast)))))))

(defn get-symbolizers []
  ;; (merge base-symbolizers *initial-symbolizers*) might be better
  (or *initial-symbolizers* base-symbolizers))

(defn symbolize
  "Generate symbolic bytecode for AST node"
  ([ast]
   (symbolize ast (get-symbolizers)))
  ([ast symbolizers]
   (if-let [symbolizer (ast->symbolizer ast symbolizers)]
     (symbolizer ast symbolizers))))

(defn compile-fn
  "Compile fn form using base-symbolizers, emit binary to current directory, return fn"
  ([expr] (compile-fn expr
            (get-symbolizers)
            "magic.compile"))
  ([expr symbolizers] (compile-fn expr symbolizers "magic.compile"))
  ([expr symbolizers asm-name]
   (->> (-> expr
            ana/analyze
            (symbolize symbolizers))
        ; (il/assembly+module asm-name)
        il/emit!
        ::il/type-builder
        .Name
        symbol
        (list 'new)
        ; Activator/CreateInstance
        )))

(defn compile-il
  "Compile bytes"
  [il]
  (-> il
      
      ))

(defmacro define [name args & body]
  (let [form (list* 'fn args body)]
    `(def ~name
       ~(compile-fn form))))

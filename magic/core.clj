(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [magic.analyzer :as ana]
            [magic.analyzer.util :refer [var-interfaces throw!]]
            [magic.analyzer.types :as types :refer [tag clr-type non-void-clr-type best-match]]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang Var RT IFn Keyword Symbol]
           [System.IO FileInfo Path]
           [System.Reflection.Emit OpCodes]
           [System.Reflection
            CallingConventions
            BindingFlags
            TypeAttributes
            MethodAttributes
            FieldAttributes
            FieldInfo
            MethodInfo
            PropertyInfo]
           System.AppDomain))

(def compile)
(def base-compilers)

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
     (il/ceq)
     (il/ldc-i4-0)
     (il/ceq)]
    
    (and (= from Boolean)
         (= to Object))
    (let [istrue (il/label)
          end (il/label)]
      [(il/brtrue istrue)
       (il/ldsfld (interop/field Magic.Constants "False"))
       (il/br end)
       istrue
       (il/ldsfld (interop/field Magic.Constants "True"))
       end])
    
    ;; convert void to nil
    ;; TODO is this a terrible idea?
    (and (= System.Void from) (not (.IsValueType to)))
    (il/ldnull)
    
    (and (= System.Void from) (.IsValueType to))
    (throw (Exception. (str "Cannot convert void to value type " to)))
    
    ;; use user defined implicit conversion if it exists
    (interop/method to "op_Implicit" from)
    (il/call (interop/method to "op_Implicit" from))
    
    ;; use user defined explicit conversion if it exists
    (interop/method to "op_Explicit" from)
    (il/call (interop/method to "op_Explicit" from))
    
    ;; use intrinsic conv opcodes from primitive to primitive
    (and (.IsPrimitive from) (.IsPrimitive to))
    (intrinsic-conv to)
    
    ;; box valuetypes to objects
    (and (.IsValueType from) (= to Object))
    (il/box from)
    
    ;; RT casts
    (and (= from Object) (= to Single))
    (il/call (if *unchecked-math*
               (interop/method RT "uncheckedFloatCast" from)
               (interop/method RT "floatCast" from)))
    (and (= from Object) (= to Double))
    (il/call (if *unchecked-math*
               (interop/method RT "uncheckedDoubleCast" from)
               (interop/method RT "doubleCast" from)))
    (and (= from Object) (= to Int32))
    (il/call (if *unchecked-math*
               (interop/method RT "uncheckedIntCast" from)
               (interop/method RT "intCast" from)))
    (and (= from Object) (= to Int64))
    (il/call (if *unchecked-math*
               (interop/method RT "uncheckedLongCast" from)
               (interop/method RT "longCast" from)))
    
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
    
    ;; emit ToString when possible
    (= to String)
    [(reference-to-type from)
     ((if (.IsValueType from)
        il/call
        il/callvirt)
      (interop/method from "ToString"))]
    
    
    :else
    (throw (Exception. (str "Cannot convert " from " to " to)))))

(defn load-argument-address [arg-id]
  (cond
    (< arg-id 16) (il/ldarga-s (byte arg-id))
    :else (il/ldarga arg-id)))

(defn load-argument-standard [arg-id]
  (cond
    (= arg-id 0) (il/ldarg-0)
    (= arg-id 1) (il/ldarg-1)
    (= arg-id 2) (il/ldarg-2)
    (= arg-id 3) (il/ldarg-3)
    (< arg-id 16) (il/ldarg-s (byte arg-id)) ;; TODO what is the cutoff?
    :else (il/ldarg arg-id)))

(defn load-integer [k]
  (cond
    (= k 0)  (il/ldc-i4-0)
    (= k 1)  (il/ldc-i4-1)
    (= k -1) (il/ldc-i4-m1)
    (< k 128) (il/ldc-i4-s (byte k))
    :else (il/ldc-i4 (int k))))

(defn load-argument [{:keys [arg-id by-ref?]}]
  (if by-ref?
    (load-argument-address arg-id)
    (load-argument-standard arg-id)))

(defn new-array [items]
  [(load-constant (int (count items)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           c
           (il/stelem-ref)])
        (range)
        items)])

(defn prepare-array [items compilers]
  (new-array
    (map
      (fn [c]
        [(compile c compilers)
         (convert (clr-type c) Object)])
       items)))

(defmulti load-constant type)

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " (type k)))

(defmethod load-constant nil [k]
  (il/ldnull))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (load-integer k))

(defmethod load-constant Int64 [k]
  (il/ldc-i8 k))

(defmethod load-constant UInt64 [k]
  (il/ldc-i8 k))

(defmethod load-constant UInt32 [k]
  (load-integer k))

(defmethod load-constant Byte [k]
  (load-integer k))

(defmethod load-constant SByte [k]
  (load-integer k))

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

(defmethod load-constant Symbol [k]
  [(load-constant (.Namespace k))
   (load-constant (.Name k))
   (il/call (interop/method Symbol "intern" String String))])

;; NOTE the stock compiler looks up types using RT.classForName
;; if the type is not a valuetype. why? does it make a difference?
(defmethod load-constant Type [v]
  [(il/ldtoken v)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defmethod load-constant
  clojure.lang.PersistentList [v]
  [(new-array (map (fn [c] [(load-constant c)
                            (convert (type c) Object)])
                   v))
   (il/castclass System.Collections.IList)
   (il/call (interop/method clojure.lang.PersistentList "create" System.Collections.IList))
   (il/castclass clojure.lang.PersistentList)])

(defmethod load-constant
  clojure.lang.PersistentVector [v]
  [(new-array (map (fn [c] [(load-constant c)
                            (convert (type c) Object)])
                   v))
   (il/call (interop/method clojure.lang.RT "vector" System.Object|[]|))
   (il/castclass clojure.lang.PersistentVector)])

(defn load-var [v]
  (let [nsname  (.. v Namespace Name ToString)
        symname (.. v Symbol ToString)]
    [(load-constant nsname)
     (load-constant symname)
     (il/call (interop/method RT "var" String String))]))

;; TODO remaining element types
(defn load-element [type]
  (case type
    ;; ??? (il/ldelem-i)
    ;; ??? (il/ldelema)
    SByte (il/ldelem-i1)
    Int16 (il/ldelem-i2)
    Int32 (il/ldelem-i4)
    Int64 (il/ldelem-i8)
    Byte (il/ldelem-u1)
    UInt16 (il/ldelem-u2)
    UInt32 (il/ldelem-u4)
    UInt64 (il/ldelem-i8) ;; weird??
    Single (il/ldelem-r4)
    Double (il/ldelem-r8)
    (if (.IsValueType type)
      (il/ldelem type)
      (il/ldelem-ref))))

;; TODO remaining element types
(defn store-element [type]
  (case type
    ;; ??? (il/stelem-i)
    ;; ??? (il/stelema)
    SByte (il/stelem-i1)
    Int16 (il/stelem-i2)
    Int32 (il/stelem-i4)
    Int64 (il/stelem-i8)
    ;; Byte (il/stelem-u1)
    ;; UInt16 (il/stelem-u2)
    ;; UInt32 (il/stelem-u4)
    ;; UInt64 (il/stelem-i8) ;; weird??
    Single (il/stelem-r4)
    Double (il/stelem-r8)
    (if (.IsValueType type)
      (il/stelem type)
      (il/stelem-ref))))

(defn get-var [v]
  (if (.isDynamic v)
    (il/call (interop/method Var "get"))
    (il/call (interop/method Var "getRawRoot"))))

(def ^:static TRUE true)
(def ^:static FALSE false)

;; TODO keep an eye on this
;; TODO il/ldarga, il/ldarga-s for references to args
(defn reference-to-type [t]
  (when (.IsValueType t)
    (let [local (il/local t)]
      [(il/stloc local)
       (il/ldloca local)])))

(defn reference-to-argument [{:keys [arg-id] :as ast}]
  (if (.IsValueType (clr-type ast))
    (load-argument-address arg-id)
    (load-argument-standard arg-id)))

(defn reference-to [{:keys [local arg-id] :as ast}]
  (if (= local :arg)
    (reference-to-argument ast)
    (reference-to-type (clr-type ast))))

(defn statement? [{{:keys [context]} :env}]
  (= context :ctx/statement))

(defn cleanup-stack
  "il/pop if in a non-void statement context.
  Required to keep the stack balanced."
  [{:keys [op] :as ast}]
  (if (and (not= :do op)  ;; do cleans up its own stack
           (not= :let op) ;; let contains a do
           (statement? ast)
           (not= System.Void (clr-type ast)))
    (il/pop)))

;;; compilers

(defn compile-reference-to [{:keys [local] :as ast} compilers]
  (if (= local :arg)
    (reference-to ast)
    [(compile ast compilers)
     (reference-to ast)]))

(defn do-compiler
  [{:keys [statements ret] :as ast} compilers]
  [(interleave
     (map #(compile % compilers) statements)
     (map #(cleanup-stack %) statements))
   (compile ret compilers)
   (cleanup-stack ret)])

(defn const-compiler
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast} compilers]
  (load-constant val))

(defn vector-compiler
  [{:keys [items]} compilers]
  [(load-constant (int (count items)))
   (prepare-array items compilers)
   (il/call (interop/method clojure.lang.RT "vector" |System.Object[]|))])

(defn set-compiler
  [{:keys [items]} compilers]
  [(load-constant (int (count items)))
   (prepare-array items compilers)
   (il/call (interop/method clojure.lang.RT "set" |System.Object[]|))])

(defn map-compiler
  [{:keys [keys vals]} compilers]
  [(load-constant (int (+ (count keys) (count vals))))
   (prepare-array (interleave keys vals) compilers)
   (il/call (interop/method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))])

(defn quote-compiler
  [{:keys [expr]} compilers]
  (load-constant (:val expr)))

(defn static-property-compiler
  "Symbolic bytecode for static properties"
  [{:keys [property]} compilers]
  (il/call (.GetGetMethod property)))

(defn instance-property-compiler
  "Symbolic bytecode for instance properties"
  [{:keys [target property]} compilers]
  [(compile-reference-to target compilers)
   (if (-> target clr-type .IsValueType)
     (il/call (.GetGetMethod property))
     (il/callvirt (.GetGetMethod property)))])

(defn static-field-compiler
  "Symbolic bytecode for static fields"
  [{:keys [field]} compilers]
  (if (.IsLiteral field)
    (load-constant (.GetRawConstantValue field))
    (il/ldsfld field)))


(defn instance-field-compiler
  "Symbolic bytecode for instance fields"
  [{:keys [field target]} compilers]
  [(compile-reference-to target compilers)
   (il/ldfld field)])

(defn dynamic-field-compiler
  "Symbolic bytecode for dynamic fields"
  [{:keys [field target]} compilers]
  (let [target-local (il/local)
        target-type (il/local Type)
        reflected-field (il/local FieldInfo)
        valid-field (il/label)]
    [(compile-reference-to target compilers)
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

(defn static-method-compiler
  "Symbolic bytecode for static methods"
  [{:keys [method args] :as ast} compilers]
  (let [arg-types (map clr-type args)]
    [(interleave
       (map #(compile % compilers) args)
       (map convert
            arg-types
            (interop/parameter-types method)))
     (il/call method)]))

(defn instance-method-compiler
  "Symbolic bytecode for instance methods"
  [{:keys [method target args generic-parameters] :as ast} compilers]
  (let [arg-types (map clr-type args)
        virtcall (if (.IsValueType (clr-type target))
                   il/call
                   il/callvirt )]
    [(compile-reference-to target compilers)
     (interleave
       (map #(compile % compilers) args)
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
       (il/call method generic-parameters))]))

(defn initobj-compiler
  "Symbolic bytecode for zero arity value type constructor invocation"
  [{:keys [type args]} compilers]
  (let [loc (il/local type)]
    [(il/ldloca-s loc)
     (il/initobj type)
     (il/ldloc loc)]))

(defn new-compiler
  "Symbolic bytecode for constructor invocation"
  [{:keys [type constructor args] :as ast} compilers]
  (let [arg-types (map clr-type args)]
    [(interleave
       (map #(compile % compilers) args)
       (map convert
            arg-types
            (interop/parameter-types constructor)))
     (il/newobj constructor)]))

(defn let-compiler
  [{:keys [bindings body loop-id] :as ast} compilers]
  (let [;; uniqued names -> il/locals
        binding-map (reduce (fn [m binding]
                              (assoc m
                                (-> binding :name)
                                (il/local (non-void-clr-type binding)
                                          (str (-> binding :name)))))
                            (sorted-map) 
                            bindings)
        binding-vector (mapv #(binding-map (-> % :name)) bindings)
        recur-target (il/label)
        ;; TODO compiler local and recur with compilers or cmplrs?
        specialized-compilers
        (merge compilers
               {:local
                (fn let-local-compiler
                  [{:keys [name by-ref?] {:keys [locals]} :env :as ast} cmplrs]
                  (if-let [loc (-> name binding-map)]
                    (if by-ref?
                      (il/ldloca loc)
                      (il/ldloc loc))
                    (compile ast compilers)))}
               (when loop-id
                 {:recur
                  (fn let-recur-compiler
                    [{:keys [exprs] :as ast} cmplrs]
                    (let [expr-range (range (count exprs))
                          temporaries
                          (mapv #(il/local (::il/type (nth binding-vector %)))
                                expr-range)]
                      [(interleave
                         (map #(compile % cmplrs) exprs)
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
            [(compile binding specialized-compilers)
             (convert (clr-type (-> binding :init)) (non-void-clr-type binding))
             (il/stloc (binding-map (:name binding)))])
          bindings)
     ;; mark recur target
     (when loop-id
       recur-target)
     ;; emit body with specialized compilers
     (compile body specialized-compilers)]))

(defn if-compiler
  [{:keys [test then else] :as ast} compilers]
  (let [if-expr-type (clr-type ast)
        then-label (il/label)
        end-label (il/label)
        value-used? (not (statement? ast))]
    (cond (types/always-then? ast) (compile then compilers)
          (types/always-else? ast) (compile else compilers)
          :else [(compile test compilers)
                 (convert (clr-type test) Boolean)
                 (il/brtrue then-label)
                 (compile else compilers)
                 (when (and value-used?
                            (not (types/control-flow? else)))
                   (convert (clr-type else) if-expr-type))
                 (il/br end-label)
                 then-label
                 (compile then compilers)
                 (when (and value-used?
                            (not (types/control-flow? then)))
                   (convert (clr-type then) if-expr-type))
                 end-label])))

(defn binding-compiler
  [{:keys [init] :as ast} compilers]
  (compile init compilers))

(defn local-compiler
  [{:keys [name local] :as ast} compilers]
  (if (= local :arg)
    (load-argument ast)
    (throw! "Local " name " not an argument and could not be compiled")))

(defn invoke-compiler
  [{:keys [fn args] :as ast} compilers]
  (let [fn-tag (-> fn :var tag)
        arg-types (map clr-type args)
        ;; TODO magic's Function interfaces should be in their own namespace
        ;; e.g. Magic.Function. Check for that instead of nil? here 
        target-interfaces (->> fn
                               var-interfaces
                               (filter #(nil? (.Namespace %)))) 
        exact-match (->> target-interfaces
                         (filter #(= (drop 1 (.GetGenericArguments %))
                                     arg-types))
                         first)]
    [(compile fn compilers)
     (if exact-match
       [(il/castclass exact-match)
        (interleave
          (map #(compile % compilers) args)
          (map #(convert (clr-type %1) %2) args arg-types))
        (il/callvirt (apply interop/method exact-match "invoke" arg-types))]
       [(il/castclass IFn)
        (interleave
          (map #(compile % compilers) args)
          (map #(convert (clr-type %) Object) args))
        (il/callvirt (apply interop/method IFn "invoke" (repeat (count args) Object)))
        (when fn-tag
          (convert Object fn-tag))])]))

(defn var-compiler
  [{:keys [var] :as ast} compilers]
  [(load-var var)
   (get-var var)])


(defn set!-compiler
  [{:keys [target val] :as ast} compilers]
  (let [target-op (:op target)
        target' (-> target :target)
        field (-> target :field)
        property (-> target :property)
        value-used? (not (statement? ast))]
    (cond
      (= target-op :instance-field)
      (let [v (il/local (clr-type val))]
        [(compile-reference-to target' compilers)
         (compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (clr-type val) (.FieldType field))
         (il/stfld field)
         (if value-used?
           (il/ldloc v))])
      (= target-op :instance-property)
      (let [v (il/local (clr-type val))]
        [(compile-reference-to target' compilers)
         (compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (clr-type val) (.PropertyType property))
         (il/callvirt (.GetSetMethod property))
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-field)
      (let [v (il/local (clr-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (clr-type val) (.FieldType field))
         (il/stsfld field)
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-property)
      (let [v (il/local (clr-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (clr-type val) (.PropertyType property))
         (il/call (.GetSetMethod property))
         (if value-used?
           (il/ldloc v))]))))

(defn has-arity-method
  "Symbolic bytecode for the IFnArity.HasArity method"
  [arities]
  (il/method
    "HasArity"
    (enum-or MethodAttributes/Public
             MethodAttributes/Virtual)
    Boolean [(il/parameter Int32)]
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

(defn gen-fn-name [n]
  (string/replace
    (str (gensym
           (str "magic$$"
                *ns* "$$"
                (or n "--anonymous--")
                "$$")))
    "."
    "$"))

(defn fn-compiler
  [{:keys [local methods raw-forms] :as ast} compilers]
  (let [arities (map :fixed-arity methods)
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
      (gen-fn-name (:form local))
      TypeAttributes/Public
      interfaces
      clojure.lang.AFunction
      [default-constructor
       (has-arity-method arities)
       (map #(compile % compilers) methods)])))

(defn fn-method-compiler
  [{:keys [body params form] {:keys [ret statements]} :body} compilers]
  (let [param-hint (-> form first tag)
        param-types (mapv clr-type params)
        obj-params (mapv (constantly Object) params)
        param-il (map #(il/parameter (clr-type %) (-> % :form str)) params)
        param-il-unhinted (map #(il/parameter Object (-> % :form str)) params)
        return-type (or param-hint
                        (non-void-clr-type ret))
        public-virtual (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        ;; void -> ret conversion happens in hinted method
        ret-type (clr-type ret)
        unhinted-ret-type (if (= ret-type System.Void) Object ret-type) 
        unhinted-method
        (il/method
          "invoke"
          public-virtual
          Object param-il-unhinted
          [(compile body compilers)
           (convert ret-type Object)
           (il/ret)])
        hinted-method
        (il/method
          "invoke"
          public-virtual
          return-type param-il
          [(compile body compilers)
           (convert ret-type return-type)
           (il/ret)])
        unhinted-shim
        (il/method
          "invoke"
          public-virtual
          Object param-il-unhinted
          [(il/ldarg-0)
           (interleave
             (map (comp load-argument-standard inc) (range))
             (map #(convert Object %) param-types))
           (il/callvirt hinted-method)
           (convert unhinted-ret-type Object)
           (il/ret)])]
    [(if (and (= param-types obj-params)
              (= return-type Object))
       unhinted-method
       [hinted-method unhinted-shim])]))

(defn try-compiler
  [{:keys [catches body finally] :as ast} compilers]
  (if (and (empty? catches)
           (nil? finally))
    (compile body compilers)
    (let [expr-type (clr-type ast)
          try-local (il/local expr-type)]
      [(il/exception
         [(if (= expr-type System.Void)
            [(compile body compilers)
             (map #(compile % compilers) catches)]
            [(compile body compilers)
             (convert (clr-type body) expr-type)
             (il/stloc try-local)
             (interleave
               (map #(compile % compilers) catches)
               (map #(convert (clr-type %) expr-type) catches)
               (repeat (il/stloc try-local)))])
          [(when finally
             (il/finally
               (compile finally compilers)))]])
       (when-not (= expr-type System.Void)
         (il/ldloc try-local))])))

(defn catch-compiler
  [{:keys [class local body]} compilers]
  (let [catch-local-name (:name local)
        catch-local (il/local (-> class :val))
        specialized-compilers
        (merge compilers
               {:local
                (fn catch-local-compiler
                  [{:keys [name by-ref?] {:keys [locals]} :env :as ast} cmplrs]
                  (if (= name (:name local))
                    (if by-ref?
                      (il/ldloca catch-local)
                      (il/ldloc catch-local))
                    (compile ast compilers)))})]
    (il/catch
      (-> class :val)
      [(il/stloc catch-local)
       (compile body specialized-compilers)])))

(defn throw-compiler
  [{:keys [exception]} compilers]
  [(compile exception compilers)
   (il/throw)])

(defn monitor-enter-compiler
  [{:keys [target]} compilers]
  [(compile target compilers)
   (convert (clr-type target) Object)
   (il/call (interop/method System.Threading.Monitor "Enter" Object))])

(defn monitor-exit-compiler
  [{:keys [target]} compilers]
  [(compile target compilers)
   (convert (clr-type target) Object)
   (il/call (interop/method System.Threading.Monitor "Exit" Object))])

(defn intrinsic-compiler
  [{:keys [original type il-fn]} compilers]
  (il-fn original type compilers))

(def base-compilers
  {:const               #'const-compiler
   :do                  #'do-compiler
   :vector              #'vector-compiler
   :set                 #'set-compiler
   :map                 #'map-compiler
   :quote               #'quote-compiler
   :fn                  #'fn-compiler
   :if                  #'if-compiler
   :let                 #'let-compiler
   :loop                #'let-compiler
   :local               #'local-compiler
   :binding             #'binding-compiler
   :invoke              #'invoke-compiler
   :var                 #'var-compiler
   :the-var             #'var-compiler
   :set!                #'set!-compiler
   :try                 #'try-compiler
   :catch               #'catch-compiler
   :throw               #'throw-compiler
   :monitor-enter       #'monitor-enter-compiler
   :monitor-exit        #'monitor-exit-compiler
   :fn-method           #'fn-method-compiler
   :static-property     #'static-property-compiler
   :instance-property   #'instance-property-compiler
   :static-field        #'static-field-compiler
   :instance-field      #'instance-field-compiler
   :dynamic-field       #'dynamic-field-compiler
   :static-method       #'static-method-compiler
   :instance-method     #'instance-method-compiler
   :initobj             #'initobj-compiler
   :new                 #'new-compiler
   :intrinsic           #'intrinsic-compiler
   })

(def ^:dynamic *spells* [])

(defn ast->compiler
  "Look up compiler for AST node. Throws exception if not found."
  [ast compilers]
  (or (-> ast :op compilers)
      (throw (Exception. (str "No compiler for " (pr-str (or  (:op ast)
                                                                ast)))))))

;; (merge base-compilers *initial-compilers*) might be better
(defn get-compilers
  ([] (get-compilers base-compilers))
  ([compilers] (get-compilers compilers *spells*))
  ([compilers spells]
   (reduce
     (fn [compilers* spell] (spell compilers*))
     compilers
     spells)))

(defn compile
  "Generate symbolic bytecode for AST node"
  ([ast]
   (compile ast (get-compilers)))
  ([ast compilers]
   (if-let [compiler (ast->compiler ast compilers)]
     (compiler ast compilers))))
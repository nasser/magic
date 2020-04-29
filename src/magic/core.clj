(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [magic.analyzer.util :refer [var-interfaces var-type throw!]]
            [magic.analyzer.types :as types :refer [tag ast-type non-void-ast-type]]
            [magic.analyzer.binder :refer [select-method]]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang Var RT IFn Keyword Symbol]
           [System.IO FileInfo Path]
           [System.Runtime.CompilerServices IsVolatile]
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

(def ^:dynamic *module* nil)

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
    (< arg-id Byte/MaxValue) (il/ldarg-s (byte arg-id))
    :else (il/ldarg arg-id)))

(defn store-argument [arg-id]
  (cond
    (< arg-id Byte/MaxValue) (il/starg-s (byte arg-id))
    :else (il/starg arg-id)))

(defn load-integer [k]
  (cond
    (= k 0)  (il/ldc-i4-0)
    (= k 1)  (il/ldc-i4-1)
    (= k -1) (il/ldc-i4-m1)
    (and (pos? k) (< k 128)) (il/ldc-i4-s (byte k))
    :else (il/ldc-i4 (int k))))

(defn load-argument [{:keys [arg-id by-ref?]}]
  (if by-ref?
    (load-argument-address arg-id)
    (load-argument-standard arg-id)))

(defn reference-to-type [t]
  (when (.IsValueType t)
    (let [loc (il/local t)]
      [(il/stloc loc)
       (il/ldloca loc)])
    #_ (il/box t)
    ))

(defn reference-to-argument [{:keys [arg-id] :as ast}]
  (if (.IsValueType (ast-type ast))
    (load-argument-address arg-id)
    (load-argument-standard arg-id)))

(defn reference-to [{:keys [local arg-id] :as ast}]
  (if (= local :arg)
    (reference-to-argument ast)
    (reference-to-type (ast-type ast))))

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
    (= from :magic.analyzer.types/disregard)
    nil ; (throw (Exception. "cannot convert from disregarded type"))
    (= to :magic.analyzer.types/disregard)
    (throw (Exception. "cannot convert to disregarded type"))
    (nil? from)
    nil

    ;; do nothing if the types are the same
    (= from to)
    nil

    (.IsEnum from)
    (convert (Enum/GetUnderlyingType from) to)

    (.IsEnum to)
    (convert from (Enum/GetUnderlyingType to))

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
    
    (and (= System.Void from) (not (.IsValueType to)))
    (il/ldnull)

    (and (= System.Void to) (not= System.Void from))
    (il/pop)
    
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
    #_(let [fail (il/label)
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

    (and (.IsValueType from)
         (.IsAssignableFrom to from))
    [(reference-to-type from)
     (il/box from)]
    
    (.IsAssignableFrom to from)
    nil

    (.IsAssignableFrom from to)
    (il/castclass to)

    ;; emit ToString when possible
    (= to String)
    [(reference-to-type from)
     ((if (.IsValueType from)
        il/call
        il/callvirt)
      (interop/method from "ToString"))]


    :else
    (throw (Exception. (str "Cannot convert " from " to " to)))))

(defmulti load-constant type)

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
         (convert (ast-type c) Object)])
       items)))

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " k " (" (type k) ")" ))

(defmethod load-constant nil [k]
  (il/ldnull))

(defmethod load-constant clojure.lang.PersistentList+EmptyList [k]
  (il/ldsfld (interop/field clojure.lang.PersistentList "EMPTY")))

(defmethod load-constant System.Text.RegularExpressions.Regex [re]
  [(il/ldstr (str re))
   (il/newobj (interop/constructor System.Text.RegularExpressions.Regex String))])

(defmethod load-constant Char [c]
  (il/ldc-i4 (int c)))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (load-integer k))

(defmethod load-constant Int16 [k]
  (il/ldc-i4 (int k)))

(defmethod load-constant UInt16 [k]
  (il/ldc-i4 (int k)))

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

(defmethod load-constant clojure.lang.Ratio [r]
  [(load-constant (str r))
   (il/call (interop/method clojure.lang.LispReader "MatchNumber" String))
   (il/castclass clojure.lang.Ratio)])

(defmethod load-constant clojure.lang.BigInt [k]
  (if (nil? (.Bipart k))
    [(load-constant (.Lpart k))
     (il/call (interop/method clojure.lang.BigInt "fromLong" Int64))]
    [(load-constant (str k "N"))
     (il/call (interop/method clojure.lang.LispReader "MatchNumber" String))
     (il/castclass clojure.lang.BigInt)]))

(defmethod load-constant clojure.lang.BigDecimal [k]
  [(il/ldstr (str k))
   (il/call (interop/method clojure.lang.BigDecimal "Parse" String))])

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

(defn persistent-list-il [v]
  (let [method (interop/method clojure.lang.PersistentList "create" System.Collections.IList)]
    [(new-array (map (fn [c] [(load-constant c)
                              (convert (type c) Object)])
                     v))
     (il/castclass System.Collections.IList)
     (il/call method)
     (convert (.ReturnType method) (types/data-structure-types :seq))]))

(defmethod load-constant
  clojure.lang.ISeq [v]
  (persistent-list-il v))

(defmethod load-constant
  clojure.lang.APersistentVector [v]
  (let [method (interop/method clojure.lang.RT "vector" System.Object|[]|)]
    [(new-array (map (fn [c] [(load-constant c)
                              (convert (type c) Object)])
                     v))
     (il/call method)
     (convert (.ReturnType method) (types/data-structure-types :vector))]))

(defmethod load-constant
  clojure.lang.APersistentSet [v]
  (let [method (interop/method clojure.lang.RT "set" System.Object|[]|)]
    [(new-array (map (fn [c] [(load-constant c)
                              (convert (type c) Object)])
                     v))
     (il/call method)
     (convert (.ReturnType method) (types/data-structure-types :set))]))

(defmethod load-constant
  clojure.lang.APersistentMap [v]
  (let [method (interop/method clojure.lang.RT "mapUniqueKeys" System.Object|[]|)]
    [(->> (interleave (keys v) (vals v))
          (map (fn [k]
                 [(load-constant k)
                  (convert (type k) Object) ]))
          new-array)
     (il/call method)
     (convert (.ReturnType method) (types/data-structure-types :map))]))

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

(defn statement? [{{:keys [context]} :env}]
  (= context :ctx/statement))

(defn cleanup-stack
  "il/pop if in a non-void statement context.
  Required to keep the stack balanced."
  [{:keys [op] :as ast}]
  (when (and (statement? ast)
             (not= System.Void (ast-type ast)))
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
   (compile ret compilers)])

(defn const-compiler
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast} compilers]
  (load-constant val))

(defn vector-compiler
  [{:keys [items] :as ast} compilers]
  (let [method (interop/method clojure.lang.RT "vector" |System.Object[]|)]
    [(prepare-array items compilers)
     (il/call method)
     (convert (.ReturnType method) (ast-type ast))]))

(defn set-compiler
  [{:keys [items] :as ast} compilers]
  (let [method (interop/method clojure.lang.RT "set" |System.Object[]|)]
    [(prepare-array items compilers)
     (il/call method)
     (convert (.ReturnType method) (ast-type ast))]))

(defn map-compiler
  [{:keys [keys vals] :as ast} compilers]
  (let [method (interop/method clojure.lang.RT "mapUniqueKeys" |System.Object[]|)]
    [(prepare-array (interleave keys vals) compilers)
     (il/call method)
     (convert (.ReturnType method) (ast-type ast))]))

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
   (if (-> target ast-type .IsValueType)
     (il/call (.GetGetMethod property))
     (il/callvirt (.GetGetMethod property)))])

(defn static-field-compiler
  "Symbolic bytecode for static fields"
  [{:keys [field]} compilers]
  (if (.IsLiteral field)
    (load-constant (.GetRawConstantValue field))
    (il/ldsfld field)))

(defn field-volatile? [f]
  (let [modifiers (into #{} (.GetRequiredCustomModifiers f))]
    (modifiers IsVolatile)))

(defn instance-field-compiler
  "Symbolic bytecode for instance fields"
  [{:keys [field target]} compilers]
  [(compile-reference-to target compilers)
   (when (field-volatile? field)
     (il/volatile))
   (il/ldfld field)])

(defn dynamic-field-compiler
  "Symbolic bytecode for dynamic fields"
  [{:keys [field target]} compilers]
  [(compile-reference-to target compilers)
   (load-constant (str field))
   (il/call (interop/method Magic.Dispatch "InvokeZeroArityMember" Object String))])

(defn dynamic-zero-arity-compiler
  "Symbolic bytecode for dynamic fields"
  [{:keys [:m-or-f target]} compilers]
  [(compile-reference-to target compilers)
   (load-constant (str m-or-f))
   (il/call (interop/method Magic.Dispatch "InvokeZeroArityMember" Object String))])

(defn static-method-compiler
  "Symbolic bytecode for static methods"
  [{:keys [method args] :as ast} compilers]
  (let [arg-types (map ast-type args)]
    [(interleave
       (map #(compile % compilers) args)
       (map convert
            arg-types
            (interop/parameter-types method)))
     (il/call method)]))

(defn instance-method-compiler
  "Symbolic bytecode for instance methods"
  [{:keys [method non-virtual? target args generic-parameters] :as ast} compilers]
  (let [arg-types (map ast-type args)
        virtcall (if (or non-virtual? (.IsValueType (ast-type target)))
                   il/call
                   il/callvirt )]
    [(compile target compilers)
     (interleave
      (map #(compile % compilers) args)
      (map convert
           arg-types
           (interop/parameter-types method)))
     (cond
       (and (.IsVirtual method)
            (nil? generic-parameters))
       [(virtcall method)]
       
       (and (not (.IsVirtual method))
            (nil? generic-parameters))
       (il/call method)
       ;; TODO ???
       (and (.IsVirtual method)
            generic-parameters)
       [(virtcall method generic-parameters)]
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
  (let [arg-types (map ast-type args)]
    [(interleave
      (map #(compile % compilers) args)
      (map convert
           arg-types
           (interop/parameter-types constructor)))
     (il/newobj constructor)]))

(defn with-meta-compiler
  "Symbolic bytecode for expressions wrapped in with-meta"
  [{:keys [expr meta] :as ast} compilers]
  (let [expr-type (ast-type expr)
        meta-type (ast-type meta)]
    [(compile expr compilers)
     (convert expr-type clojure.lang.IObj)
     (il/castclass clojure.lang.IObj)
     (compile meta compilers)
     (convert meta-type clojure.lang.IPersistentMap)
     (il/callvirt (interop/method clojure.lang.IObj "withMeta" clojure.lang.IPersistentMap))]))

(defn loop-compiler
  [{:keys [bindings body] :as ast} compilers]
  (let [;; uniqued names -> il/locals
        binding-map 
        (reduce (fn [m binding]
                  (assoc m
                         (-> binding :name)
                         (il/local (non-void-ast-type binding)
                                   (str (-> binding :name)))))
                (sorted-map)
                bindings)
        binding-vector (mapv #(binding-map (-> % :name)) bindings)
        recur-target (il/label)
        ;; TODO compiler local and recur with compilers or cmplrs?
        specialized-compilers
        (merge compilers
               {:local (fn loop-local-compiler
                         [{:keys            [name init by-ref?]
                           :as              ast} cmplrs]
                         (if-let [loc (-> name binding-map)]
                           (if by-ref?
                             (il/ldloca loc)
                             [(il/ldloc loc)
                              #_ (convert (ast-type init) (ast-type ast))
                              ])
                           (compile ast compilers)))}
               {:recur (fn loop-recur-compiler
                         [{:keys [exprs]
                           :as   ast} cmplrs]
                         [(interleave 
                           (map #(compile % cmplrs) exprs)
                           (map #(convert (ast-type %1) (non-void-ast-type %2)) exprs bindings))
                          (map il/stloc (reverse binding-vector))
                          (il/br recur-target)])})]
    ;; emit local initializations
    [(map (fn [binding]
            [(compile binding specialized-compilers)
             (convert (ast-type (-> binding :init)) (non-void-ast-type binding))
             (il/stloc (binding-map (:name binding)))])
          bindings)
     ;; mark recur target
     recur-target
     ;; emit body with specialized compilers
     (compile body specialized-compilers)]))

(defn let-compiler
  [{:keys [bindings body] :as ast} compilers]
  (let [;; uniqued names -> il/locals
        binding-map (reduce (fn [m binding]
                              (assoc m
                                     (:name binding)
                                     (il/local (non-void-ast-type binding)
                                               (str (:name binding)))))
                            (sorted-map) 
                            bindings)
        ;; TODO compiler local and recur with compilers or cmplrs?
        specialized-compilers
        (merge compilers
               {:local
                (fn let-local-compiler
                  [{:keys [name form by-ref?] {:keys [locals]} :env :as ast} cmplrs]
                  (if-let [loc (-> name binding-map)]
                    (if by-ref?
                      (il/ldloca loc)
                      [(il/ldloc loc)
                       (when (-> form locals :init)
                         (convert (ast-type (-> form locals :init)) (non-void-ast-type ast)))])
                    (compile ast compilers)))})]
    ;; emit local initializations
    [(map (fn [binding]
            [(compile binding specialized-compilers)
             (convert (ast-type (-> binding :init)) (non-void-ast-type binding))
             (il/stloc (binding-map (:name binding)))])
          bindings)
     ;; emit body with specialized compilers
     (compile body specialized-compilers)]))

(defn letfn-compiler
  [{:keys [bindings body] :as ast} compilers]
  (let [binding-map (reduce (fn [m binding]
                              (assoc m
                                     (:name binding)
                                     (il/local (non-void-ast-type binding)
                                               (str (:name binding)))))
                            (sorted-map) 
                            bindings)
        specialized-compilers
        (merge compilers
               {:local
                (fn let-local-compiler
                  [{:keys [name form by-ref?] {:keys [locals]} :env :as ast} cmplrs]
                  (if-let [loc (-> name binding-map)]
                    (if by-ref?
                      (il/ldloca loc)
                      [(il/ldloc loc)
                       (when (-> form locals :init)
                         (convert (ast-type (-> form locals :init)) (non-void-ast-type ast)))])
                    (compile ast compilers)))})
        binding-il (map #(compile % specialized-compilers) bindings)]
    ;; emit local initializations
    [(map (fn [il binding]
            [(drop-last il)
             (convert (ast-type (-> binding :init)) (non-void-ast-type binding))
             (il/stloc (binding-map (:name binding)))])
          binding-il bindings)
     (interleave 
      (map (fn [il binding]
             [(il/ldloc (binding-map (:name binding)))
              (last il)])
           binding-il bindings)
      (repeat (il/pop)))
     (compile body specialized-compilers)]))

(defn if-compiler
  [{:keys [test then else] :as ast} compilers]
  (let [if-expr-type (ast-type ast)
        then-label (il/label)
        end-label (il/label)]
    (cond (types/always-then? ast) (compile then compilers)
          (types/always-else? ast) (compile else compilers)
          :else [(compile test compilers)
                 (convert (ast-type test) Boolean)
                 (il/brtrue then-label)
                 (compile else compilers)
                 (when-not (types/disregard-type? else)
                   (convert (ast-type else) if-expr-type))
                 (il/br end-label)
                 then-label
                 (compile then compilers)
                 (when-not (types/disregard-type? then)
                   (convert (ast-type then) if-expr-type))
                 end-label])))

(defn binding-compiler
  [{:keys [init] :as ast} compilers]
  (compile init compilers))

(defn local-compiler
  [{:keys [name local] :as ast} compilers]
  (case local
    :fn
    [(load-argument-standard 0)
     (convert (ast-type local) (ast-type ast))]
    :proxy-this
    (load-argument-standard 0)
    (throw! "Local " name " not bound, could not compile! " local)))

(defn implementing-interface [t bm]
  (->> (.GetInterfaces t)
       ;; TODO magic's Function interfaces should be in their own namespace
       ;; e.g. Magic.Function. Check for that instead of nil? here 
       (filter #(nil? (.Namespace %)))
       (filter
         #(contains?
            (set (.. t (GetInterfaceMap %) TargetMethods))
            bm))
       first))

(defn ifn-invoke-compiler [{:keys [args] :as ast} compilers]
  (let [positional-args (take 20 args)
        rest-args (drop 20 args)
        invoke-method 
        (if (empty? rest-args)
          (apply interop/method IFn "invoke" (repeat (count args) Object))
          (apply interop/method IFn "invoke" (concat (repeat 20 Object) [System.Object|[]|])) )]
    [(il/castclass IFn)
     (interleave
      (map #(compile % compilers) positional-args)
      (map #(convert (ast-type %) Object) positional-args))
     (when-not (empty? rest-args)
       [(load-constant (count rest-args))
        (il/newarr Object)
        (map-indexed 
         (fn [i arg]
           [(il/dup)
            (load-constant (int i))
            (compile arg compilers)
            (convert (ast-type arg) Object)
            (il/stelem-ref)])
         rest-args)])
     (il/callvirt invoke-method)
     (convert Object (ast-type ast))]))

(defn invoke-compiler
  [{:keys [fn args] :as ast} compilers]
  (let [fn-type (var-type fn)
        arg-types (map ast-type args)
        ;; TODO this is hacky and gross
        best-method (when fn-type
                      (select-method (filter #(= (.Name %) "invoke")
                                             (.GetMethods fn-type))
                                     arg-types))
        param-types (when best-method
                      (map #(.ParameterType %) (.GetParameters best-method)))
        interface-match (when best-method
                          (implementing-interface fn-type best-method))]
    [(compile fn compilers)
     (if interface-match
       [(il/castclass interface-match)
        (interleave
          (map #(compile % compilers) args)
          (map #(convert %1 %2) arg-types param-types))
        (il/callvirt (apply interop/method interface-match "invoke" param-types))]
       (ifn-invoke-compiler ast compilers))]))

(defn var-compiler
  [{:keys [var] :as ast} compilers]
  [(load-var var)
   (get-var var)])

(defn the-var-compiler
  [{:keys [var] :as ast} compilers]
  [(load-var var)])


(defn set!-compiler
  [{:keys [target val] :as ast} compilers]
  (let [target-op (:op target)
        target' (-> target :target)
        field (-> target :field)
        property (-> target :property)
        value-used? (not (statement? ast))]
    (cond
      (= target-op :instance-field)
      (if (.IsInitOnly field)
        (throw (ex-info "Cannot set! immutable field" 
                        {:field (.Name field) :type (.DeclaringType field) :form (:form ast)}))
        (let [v (il/local (ast-type val))]
          [(compile-reference-to target' compilers)
           (compile val compilers)
           (when value-used?
             [(il/stloc v)
              (il/ldloc v)])
           (convert (ast-type val) (.FieldType field))
           (when (field-volatile? field)
             (il/volatile))
           (il/stfld field)
           (when value-used?
             (il/ldloc v))]))
      (= target-op :instance-property)
      (let [v (il/local (ast-type val))]
        [(compile-reference-to target' compilers)
         (compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (ast-type val) (.PropertyType property))
         (il/callvirt (.GetSetMethod property))
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-field)
      (let [v (il/local (ast-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (ast-type val) (.FieldType field))
         (il/stsfld field)
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-property)
      (let [v (il/local (ast-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert (ast-type val) (.PropertyType property))
         (il/call (.GetSetMethod property))
         (if value-used?
           (il/ldloc v))])
      (= target-op :dynamic-zero-arity)
      [(compile-reference-to target' compilers)
       (load-constant (str (-> target :m-or-f)))
       (compile val compilers)
       (convert (ast-type val) Object)
       (il/call (magic.interop/method Magic.Dispatch "SetMember" Object String Object))
       (convert Object (ast-type val))
       (when-not value-used?
         (il/pop))])))

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
               (load-constant (int arity))
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

(defn compile-fn-type [{:keys [methods closed-overs fn-type] :as ast} compilers]
  (when-not (.IsCreated fn-type)
    (let [arities (map :fixed-arity methods)
          closed-over-field-map
          (reduce-kv
           (fn [m k v]
             (assoc m k (il/unique (il/field (ast-type v) (str (:form v))
                                             FieldAttributes/Assembly))))
           (sorted-map)
           closed-overs)
          specialized-compilers
          (merge
           compilers
           {:local
            (fn fn-local-compiler
              [{:keys [name] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)]
                (local-compiler ast _cmplrs)))})
          ctor (il/constructor
                (enum-or MethodAttributes/Public)
                CallingConventions/Standard []
                [(il/ldarg-0)
                 (il/call (private-constructor clojure.lang.AFunction))
                 (il/ret)])
          methods* (map #(compile % specialized-compilers) methods)]
      (reduce (fn [ctx x] (il/emit! ctx x))
              {::il/type-builder fn-type}
              [ctor methods* (has-arity-method arities)]))
    (.CreateType fn-type)))

(defn fn-compiler
  [{:keys [fn-type closed-overs] :as ast} compilers]
  (compile-fn-type ast compilers)  
  [(il/newobj (first (.GetConstructors fn-type)))
   (map
    (fn [il field] [(il/dup) il (il/stfld field)])
    (->> closed-overs vals (map #(compile % compilers)))
    (.GetFields fn-type (enum-or BindingFlags/NonPublic BindingFlags/Instance)))])

(defn fn-method-compiler
  [{:keys [body params form]} compilers]
  (let [param-hint (-> form first tag)
        param-types (mapv ast-type params)
        param-names (into #{} (map :name params))
        obj-params (mapv (constantly Object) params)
        param-il (map #(il/parameter (ast-type %) (-> % :form str)) params)
        param-il-unhinted (map #(il/parameter Object (-> % :form str)) params)
        return-type (or param-hint (ast-type body))
        non-void-return-type (or param-hint (non-void-ast-type body))
        public-virtual (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        recur-target (il/label)
        specialized-compilers
        (merge compilers
               {:recur (fn fn-recur-compiler
                         [{:keys [exprs]
                           :as   ast} cmplrs]
                         [(interleave 
                           (map #(compile % cmplrs) exprs)
                           (map #(convert (ast-type %1) %2) exprs param-types))
                          (map store-argument (->> params count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn fn-method-local-compiler
                         [{:keys [arg-id name local by-ref?] :as ast} _cmplrs]
                         (if (and (= local :arg) 
                                  (param-names name))
                           (if by-ref?
                             (load-argument-address (inc arg-id))
                             [(load-argument-standard (inc arg-id))
                              (convert (param-types arg-id) (ast-type ast))])
                           (compile ast compilers)))})
        unhinted-method
        (il/method
         "invoke"
         public-virtual
         Object param-il-unhinted
         [recur-target
          (compile body specialized-compilers)
          (convert return-type Object)
          (il/ret)])
        hinted-method
        (il/method
         "invoke"
         public-virtual
         non-void-return-type param-il
         [recur-target
          (compile body specialized-compilers)
          (convert return-type non-void-return-type)
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
          (convert non-void-return-type Object)
          (il/ret)])]
    [(if (and (= param-types obj-params)
              (= return-type Object))
       unhinted-method
       [hinted-method unhinted-shim])]))

(defn try-compiler
  [{:keys [catches closed-overs body finally] :as ast {:keys [empty-stack?]} :env} compilers]
  (if (and (empty? catches)
           (nil? finally))
    (compile body compilers)
    (let [expr-type (ast-type ast)
          closed-overs-map
          (into {} (map (fn [name i] [name i]) (keys closed-overs) (range)))
          closure-compilers
          (merge compilers
                 {:local (fn try-local-compiler 
                           [{:keys [name] :as ast} _compilers]
                           (if-let [arg-id (closed-overs-map name)]
                             (load-argument-standard (inc arg-id))
                             (compile ast compilers)))})          
          bodyfn
          (fn [compilers]
            (let [try-local (il/local expr-type)]
              [(il/exception
                [(if (= expr-type System.Void)
                   [(compile body compilers)
                    (map #(compile % compilers) catches)]
                   [(compile body compilers)
                    (convert (ast-type body) expr-type)
                    (il/stloc try-local)
                    (interleave
                     (map #(compile % compilers) catches)
                     (map #(when-not (types/disregard-type? %) (convert (ast-type %) expr-type)) catches)
                     (repeat (il/stloc try-local)))])
                 [(when finally
                    (il/finally
                      (compile finally compilers)))]])
               (when-not (= expr-type System.Void)
                 (il/ldloc try-local))]))
          method-il
          (il/method 
           (str (gensym "try"))
           MethodAttributes/Private
           expr-type (->> closed-overs vals (mapv non-void-ast-type))
           [(bodyfn closure-compilers)
            (il/ret)])]
      (if empty-stack?
        (bodyfn compilers)
        [(load-argument-standard 0)
         (->> closed-overs vals (map #(compile % compilers)))
         (il/call method-il)]))))

(defn throw-compiler
  [{:keys [exception]} compilers]
  (when-not exception
    (throw (Exception. "throw must take an argument outside of a catch")))
  [(compile exception compilers)
   (convert (ast-type exception) Exception)
   (il/throw)])

(defn catch-compiler
  [{:keys [class local body]} compilers]
  (let [catch-local-name (:name local)
        catch-local (il/local (-> class :val))
        compilers+local
        (merge compilers
               {:local
                (fn catch-local-compiler
                  [{:keys [name by-ref?] :as ast} cmplrs]
                  (if (= name catch-local-name)
                    (if by-ref?
                      (il/ldloca catch-local)
                      (il/ldloc catch-local))
                    (compile ast compilers)))})
        specialized-compilers
        (merge compilers+local
               {:throw
                (fn catch-throw-compiler
                  [{:keys [exception] :as ast} cmplrs]
                  (if-not exception
                    (il/rethrow)
                    (throw-compiler ast compilers+local)))})]
    (il/catch
     (-> class :val)
     [(il/stloc catch-local)
      (compile body specialized-compilers)])))

(defn monitor-enter-compiler
  [{:keys [target]} compilers]
  [(compile target compilers)
   (convert (ast-type target) Object)
   (il/call (interop/method System.Threading.Monitor "Enter" Object))])

(defn monitor-exit-compiler
  [{:keys [target]} compilers]
  [(compile target compilers)
   (convert (ast-type target) Object)
   (il/call (interop/method System.Threading.Monitor "Exit" Object))])

(defn intrinsic-compiler
  [{:keys [original type il-fn]} compilers]
  (il-fn original type compilers))


(defn case-compiler
  [{:keys [shift mask default switch-type local mode skip-check switch-values tests expressions] :as ast} compilers]
  (let [skip-check (or skip-check #{})
        smallest-switch-value (apply min switch-values)
        largest-switch-value (apply max switch-values)
        labels (vec (repeatedly (count switch-values) #(il/label)))
        ;; idk why we need two labels here, this looks like a MAGE bug
        default-label (il/label)
        default-label2 (il/label)
        return-label (il/label)
        next-labels (drop 1 (conj labels default-label2))
        label-map (zipmap (map #(- % smallest-switch-value) switch-values) labels)
        jump-table-size (inc (- largest-switch-value smallest-switch-value))
        jump-table (map #(if-let [clause-label (label-map %)]
                           clause-label
                           default-label)
                        (range jump-table-size))
        expr-type (ast-type ast)
        local-type (ast-type local)
        sparse? (= switch-type :sparse)
        match-value-il
        (case mode
          :int
          [(cond
             (types/integer-type? local-type)
             [(compile local compilers)
              (convert local-type Int32)]
             (= Char local-type)
             (il/br default-label2)
             :else
             [(compile local compilers)
              (convert local-type Object)
              (il/call (interop/method clojure.lang.Util "IsNonCharNumeric" Object))
              (il/brfalse default-label2)
              (compile local compilers)
              (convert local-type Object)
              (il/call (interop/method clojure.lang.Util "ConvertToInt" Object))])]
          (:hash-equiv :hash-identity)
          [(compile local compilers)
           (convert local-type Object)
           (il/call (interop/method clojure.lang.Util "hash" Object))])
        switch-body-il
        (case mode
          :int
          (map
           (fn [label test expression next-label]
             [label
              (when-not (= local-type Int32)
                (let [equiv-method (or (interop/method clojure.lang.Util "equiv" local-type (or (type test) Object))
                                       (interop/method clojure.lang.Util "equiv" Object Object))
                      parameter-types (->> equiv-method .GetParameters (map #(.ParameterType %)))]
                  [(compile local compilers)
                   (convert local-type (first parameter-types))
                   (load-constant test)
                   (convert (type test) (last parameter-types))
                   (il/call equiv-method)
                   (il/brfalse next-label)]))
              (compile expression compilers)
              (convert (ast-type expression) expr-type)
              (il/br return-label)])
           labels
           tests
           expressions
           (if sparse? next-labels (repeat default-label2)))
          :hash-identity
          (map
           (fn [label test expression switch-value next-label]
             [label
              (when-not (skip-check switch-value)
                [(compile local compilers)
                 (load-constant test)
                 (il/ceq)
                 (il/brfalse next-label)])
              (compile expression compilers)
              (convert (ast-type expression) expr-type)
              (il/br return-label)])
           labels
           tests
           expressions
           switch-values
           (if sparse? next-labels (repeat default-label2)))
          :hash-equiv
          (map
           (fn [label test expression switch-value next-label]
             (let [equiv-method (or (interop/method clojure.lang.Util "equiv" local-type (or (type test) Object))
                                    (interop/method clojure.lang.Util "equiv" Object Object))
                   parameter-types (->> equiv-method .GetParameters (map #(.ParameterType %)))]
               [label
                (when-not (skip-check switch-value)
                  [(compile local compilers)
                   (convert local-type (first parameter-types))
                   (load-constant test)
                   (convert (type test) (last parameter-types))
                   (il/call equiv-method)
                   (il/brfalse next-label)])
                (compile expression compilers)
                (convert (ast-type expression) expr-type)
                (il/br return-label)]))
           labels
           tests
           expressions
           switch-values
           (if sparse? next-labels (repeat default-label2))))]
    [(when-not sparse?
       [match-value-il
        (when (pos? shift)
          [(load-constant (int shift))
           (il/shr)])
        (when (pos? mask)
          [(load-constant (int mask))
           (il/and)])
        (when-not (zero? smallest-switch-value)
          [(load-constant (int smallest-switch-value))
           (il/sub)])
        (il/switch jump-table)
        (il/br default-label2)])
     switch-body-il
     default-label ;; this looks like a MAGE bug
     default-label2
     (compile default compilers)
     (convert (ast-type default) expr-type)
     return-label]))

(defn default-override-method [method attributes]
  (il/method
   (.Name method)
   attributes
   (.ReturnType method)
   (mapv #(.ParameterType %) (.GetParameters method))
   [(il/newobj (interop/constructor NotSupportedException))
    (il/throw)]))

(defn all-abstract-methods [type]
  (->> type
       .GetProperties
       (mapcat #(vector (.GetSetMethod %) (.GetGetMethod %)))
       (concat (.GetMethods type))
       (remove nil?)
       (filter #(.IsAbstract %))))

(defn compile-proxy-type [{:keys [args super interfaces closed-overs fns proxy-type] :as ast}]
  (when-not (.IsCreated proxy-type)
    (let [super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
          iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
          super-ctor (select-method (.GetConstructors super (enum-or BindingFlags/Instance BindingFlags/Public BindingFlags/NonPublic)) (map ast-type args))
          interfaces (conj interfaces clojure.lang.IProxy)
          ;; need to gather *all* interfaces this type effectively supports
          ifaces* (into #{} (concat interfaces (mapcat #(.GetInterfaces %) interfaces)))
          iface-methods
          (->> ifaces*
               (mapcat (fn [iface]
                         (map
                          #(vector % (default-override-method % iface-override))
                          (all-abstract-methods iface))))
               (into {}))
          abstract-methods
          (into {}
                (map #(vector % (default-override-method % super-override))
                     (all-abstract-methods super)))
          closed-over-field-map
          (reduce-kv
           (fn [m k v]
             (assoc m k (il/unique (il/field (ast-type v) (str (:form v))))))
           {}
           closed-overs)
          specialized-compilers
          (merge
           base-compilers
           {:local
            (fn proxy-local-compiler
              [{:keys [name] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)]
                (local-compiler ast _cmplrs)))})
          provided-methods
          (into {} (map (fn [f] [(:source-method f) (compile f specialized-compilers)]) fns))
          methods (merge iface-methods abstract-methods provided-methods)
          arg-types (map ast-type args)
          ctor-params (concat
                       (map ast-type args)
                       (map ast-type (vals closed-overs)))
          ctor (il/constructor
                MethodAttributes/Public
                CallingConventions/Standard
                ctor-params
                [(il/ldarg-0)
                 (interleave
                  (map #(load-argument-standard (inc %2)) args (range))
                  (map convert arg-types (interop/parameter-types super-ctor)))
                 (il/call super-ctor)
                 (->> closed-over-field-map
                      vals
                      (map-indexed
                       (fn [i field]
                         [(il/ldarg-0)
                          (load-argument-standard (+ (count args) (inc i)))
                          (il/stfld field)])))
                 (il/ret)])
          ctx (reduce (fn [ctx field] (il/emit! ctx field))
                      {::il/type-builder proxy-type}
                      (vals closed-over-field-map))
          ctx (reduce (fn [ctx method] (il/emit! ctx method))
                      ctx
                      (vals methods))
          ]
      (il/emit! ctx ctor)
      (.CreateType proxy-type))))

(defn proxy-compiler [{:keys [class-and-interface proxy-type args closed-overs form] :as ast} compilers]  
  (compile-proxy-type ast)
  (when-not (every? #(and (= :const (:op %))
                          (= :class (:type %)))
                    class-and-interface)
    (throw (ex-info (str "Classes and interfaces passed to proxy must be type constants")
                    {:form form})))
  [(map #(compile % compilers) args)
   (map #(compile % compilers) (vals closed-overs))
   (il/newobj (first (.GetConstructors proxy-type)))])

(defn proxy-method-compiler [{:keys [name body source-method] :as ast} compilers]
  (let [name (str name)
        explicit-override? (string/includes? name ".")
        super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
        explicit-override (enum-or MethodAttributes/Private MethodAttributes/Virtual MethodAttributes/NewSlot)
        param-types (mapv #(.ParameterType %) (.GetParameters source-method))
        param-names (into #{} (map :name (:params ast)))
        return-type (.ReturnType source-method)
        attributes (if (.. source-method DeclaringType IsInterface)
                     (if explicit-override? explicit-override iface-override)
                     super-override)
        name (.Name source-method)
        body-type (ast-type body)
        recur-target (il/label)
        specialized-compilers
        (merge compilers
               {:recur (fn fn-recur-compiler
                         [{:keys [exprs]} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert (ast-type %1) %2) exprs param-types))
                          (map store-argument (->> param-types count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn fn-method-local-compiler
                         [{:keys [arg-id name local by-ref?] :as ast} _cmplrs]
                         (if (and (= local :arg)
                                  (param-names name))
                           (if by-ref?
                             (load-argument-address (inc arg-id))
                             [(load-argument-standard (inc arg-id))
                              (convert (param-types arg-id) (ast-type ast))])
                           (compile ast compilers)))})]
    (il/method
     name
     attributes
     return-type param-types
     (when explicit-override? source-method)
     [recur-target
      (compile body specialized-compilers)
      (convert body-type return-type)
      (il/ret)])))

(defn reify-method-compiler [{:keys [name body source-method reify-type] :as ast} compilers]
  (let [name (str name)
        explicit-override? (string/includes? name ".")
        super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        explicit-override (enum-or MethodAttributes/Private MethodAttributes/Virtual MethodAttributes/NewSlot)
        iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
        param-types (mapv #(.ParameterType %) (.GetParameters source-method))
        param-types-this (vec (concat [reify-type] param-types))
        param-names (into #{} (map :name (:params ast)))
        return-type (.ReturnType source-method)
        attributes (if (.. source-method DeclaringType IsInterface)
                     (if explicit-override? explicit-override iface-override)
                     super-override)
        body-type (ast-type body)
        recur-target (il/label)
        specialized-compilers
        (merge compilers
               {:recur (fn reify-recur-compiler
                         [{:keys [exprs]} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert (ast-type %1) %2) exprs param-types))
                          (map store-argument (->> param-types count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn reify-method-local-compiler
                         [{:keys [arg-id name local by-ref?] :as ast} _cmplrs]
                         (println "[reify-method-local-compiler]" name arg-id param-types-this param-names)
                         (if (and (= local :arg)
                                  (param-names name))
                           (if by-ref?
                             (load-argument-address arg-id)
                             [(load-argument-standard arg-id)
                              (convert (param-types-this arg-id) (ast-type ast))])
                           (compile ast compilers)))})]
    (il/method
     name
     attributes
     return-type param-types
     (when explicit-override? source-method)
     [recur-target
      (compile body specialized-compilers)
      (convert body-type return-type)
      (il/ret)])))

(defn iobj-implementation [meta-il meta-field meta-ctor closed-over-field-map]
  (let [iface-override (enum-or MethodAttributes/Public MethodAttributes/Virtual MethodAttributes/Final MethodAttributes/NewSlot)
        static-meta-field (il/field clojure.lang.IPersistentMap "<static_meta>" (enum-or FieldAttributes/Private FieldAttributes/Static))
        iobj-with-meta
        (il/method
         "withMeta"
         iface-override
         clojure.lang.IObj [clojure.lang.IPersistentMap]
         [(il/ldarg-1)
          (->> closed-over-field-map
               vals
               (map (fn [field] [(il/ldarg-0) (il/ldfld field)])))
          (il/newobj meta-ctor)
          (il/ret)])
        imeta-meta
        (il/method
         "meta"
         iface-override
         clojure.lang.IPersistentMap []
         (if-not meta-il
           [(il/ldarg-0)
            (il/ldfld meta-field)
            (il/ret)]
           (let [null-meta-label (il/label)
                 not-null-static-meta-label (il/label)]
             [(il/ldarg-0)
              (il/ldfld meta-field)
              (il/ldnull)
              (il/ceq)
              (il/brtrue null-meta-label)
              (il/ldarg-0)
              (il/ldfld meta-field)
              (il/ret)
              null-meta-label
              (il/ldsfld static-meta-field)
              (il/ldnull)
              (il/ceq)
              (il/brfalse not-null-static-meta-label)
              meta-il
              (il/stsfld static-meta-field)
              not-null-static-meta-label
              (il/ldsfld static-meta-field)
              (il/ret)])))]
    [iobj-with-meta imeta-meta]))

(defn compile-reify-type [{:keys [methods interfaces closed-overs reify-type meta] :as ast} compilers]
  (when-not (.IsCreated reify-type)
    (let [meta-field (il/field clojure.lang.IPersistentMap "<meta>")
          closed-over-field-map
          (reduce-kv
           (fn [m k v]
             (assoc m k (il/unique (il/field (ast-type v) (str (:form v))))))
           (sorted-map)
           closed-overs)
          specialized-compilers
          (merge
           compilers
           {:local
            (fn proxy-local-compiler
              [{:keys [name] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)]
                (local-compiler ast _cmplrs)))})
          super-ctor (first (.GetConstructors Object))
          ctor-params (map ast-type (vals closed-overs))
          ctor (il/constructor
                MethodAttributes/Public
                CallingConventions/Standard
                ctor-params
                [(il/ldarg-0)
                 (il/call super-ctor)
                 (->> closed-over-field-map
                      vals
                      (map-indexed
                       (fn [i field]
                         [(il/ldarg-0)
                          (load-argument-standard (inc i))
                          (il/stfld field)])))
                 (il/ret)])
          meta-ctor
          (il/constructor
           MethodAttributes/Public
           CallingConventions/Standard
           (concat [clojure.lang.IPersistentMap] ctor-params)
           [(il/ldarg-0)
            (->> closed-over-field-map
                 count
                 range
                 (map #(load-argument-standard (+ 2 %))))
            (il/call ctor)
            (il/ldarg-0)
            (il/ldarg-1)
            (il/stfld meta-field)
            (il/ret)])
          meta-il
          (when-not (nil? (:form meta))
            (compile meta specialized-compilers))
          super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
          iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
          ifaces* (disj (into #{} (concat interfaces (mapcat #(.GetInterfaces %) interfaces)))
                        clojure.lang.IObj
                        clojure.lang.IMeta)
          iface-methods
          (->> ifaces*
               (mapcat (fn [iface]
                         (map
                          #(vector % (default-override-method % iface-override))
                          (all-abstract-methods iface))))
               (into {}))
          provided-methods
          (into {} (map (fn [m] [(:source-method m) (compile m specialized-compilers)]) methods))
          methods* (merge iface-methods provided-methods)]
      (reduce (fn [ctx method] (il/emit! ctx method))
              {::il/type-builder reify-type}
              [ctor meta-ctor (vals methods*)
               (iobj-implementation meta-il meta-field meta-ctor closed-over-field-map)]))
    (.CreateType reify-type)))

(defn reify-compiler [{:keys [closed-overs reify-type] :as ast} compilers]
  (compile-reify-type ast compilers)
  [(map #(compile % compilers) (vals closed-overs))
   (il/newobj (first (.GetConstructors reify-type)))])

(defn deftype-method-compiler [{:keys [name body source-method deftype-type] :as ast} compilers]
  (let [name (str name)
        explicit-override? (string/includes? name ".")
        super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        explicit-override (enum-or MethodAttributes/Private MethodAttributes/Virtual MethodAttributes/NewSlot)
        iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
        param-types (mapv #(.ParameterType %) (.GetParameters source-method))
        param-types-this (vec (concat [deftype-type] param-types))
        param-names (into #{} (map :name (:params ast)))
        return-type (.ReturnType source-method)
        attributes (if (.. source-method DeclaringType IsInterface)
                     (if explicit-override? explicit-override iface-override)
                     super-override)
        body-type (ast-type body) ;; TODO this is a field builder for some reasons??
        recur-target (il/label)
        specialized-compilers
        (merge compilers
               {:recur (fn deftype-recur-compiler
                         [{:keys [exprs]} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert (ast-type %1) %2) exprs param-types))
                          (map store-argument (->> param-types count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn deftype-method-local-compiler
                         [{:keys [arg-id name local by-ref?] :as ast} _cmplrs]
                         (if (and (= local :arg)
                                  (param-names name))
                           (if by-ref?
                             (load-argument-address arg-id)
                             [(load-argument-standard arg-id)
                              (convert (param-types-this arg-id) (ast-type ast))])
                           (compile ast compilers)))})]
    (il/method
     name
     attributes
     return-type param-types
     (when explicit-override? source-method)
     [recur-target
      (compile body specialized-compilers)
      (convert body-type return-type)
      (il/ret)])))

(defn compile-getbasis [tb symbols]
  (let [ilg (->> tb
                 .GetMethods
                 (filter #(= "getBasis" (.Name %)))
                 first
                 .GetILGenerator)]
    (il/emit!
     {::il/ilg ilg}
     [(load-constant symbols)
      (il/ret)])))

(defn deftype-compiler [{:keys [fields methods options implements deftype-type]} compilers]
  (let [super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
        ifaces* (into #{} (concat implements (mapcat #(.GetInterfaces %) implements)))
        iface-methods
        (->> ifaces*
             (mapcat (fn [iface]
                       (map
                        #(vector % (default-override-method % iface-override))
                        (all-abstract-methods iface))))
             (into {}))
        fieldinfos (.GetFields deftype-type)
        fieldinfos-set (into #{} (.GetFields deftype-type))
        field-metas (into {} (map #(vector (str %) (meta %)) fields))
        field-map (into {} (map #(vector (.Name %) %) fieldinfos))
        ;; annoyingly, SRE does not support FieldInfo.GetRequiredCustomModifiers
        ;; on FieldBuilders, so we have to do our own bookeeping
        volatile? (fn [f] (-> f .Name field-metas :volatile-mutable))
        super-ctor (first (.GetConstructors Object))
        ctor-params (mapv #(.FieldType %) fieldinfos)
        ctor
        (il/constructor
         MethodAttributes/Public
         CallingConventions/Standard
         ctor-params
         [(il/ldarg-0)
          (il/call super-ctor)
          (->> fieldinfos
               (map-indexed
                (fn [i field]
                  [(il/ldarg-0)
                   (load-argument-standard (inc i))
                   (when (volatile? field)
                     (il/volatile))
                   (il/stfld field)])))
          (il/ret)])
        specialized-compilers
        (merge
         compilers
         {;; due to a limitation of SRE we cannot look up constructors to
          ;; TypeBuilders the way we do for normal types. so we special case
          ;; the construction of the deftype-type here to get around that.
          :new
          (fn deftype-new-compiler
            [{:keys [type args] :as ast} local-compilers]
            (if (= type deftype-type)
              (let [arg-types (map ast-type args)]
                [(interleave
                  (map #(compile % local-compilers) args)
                  (map convert
                       arg-types
                       ctor-params))
                 (il/newobj ctor)])
              (compile ast compilers)))
          :instance-field
          (fn deftype-instance-field-compiler 
            [{:keys [field] :as ast} _compilers]
            (if (fieldinfos-set field)
              [(il/ldarg-0)
               (when (volatile? field)
                 (il/volatile))
               (il/ldfld field)]
              (compile ast compilers)))
          :local
          (fn deftype-local-compiler
            [{:keys [local name] :as ast} inner-compilers]
            (if (and (= :field local)
                     (field-map (str name)))
              (compile {:op :instance-field
                        :field (field-map (str name))}
                       inner-compilers)
              (local-compiler ast compilers)))
          :set!
          (fn deftype-set!-compiler 
            [{:keys [target val] :as ast} cmplrs]
            (let [value-used? (not (statement? ast))]
              (cond (and (= :instance-field (:op target))
                         (fieldinfos-set (:field target)))
                    (let [field (:field target)
                          val-local (il/local (ast-type val))]
                      (if (.IsInitOnly field)
                        (throw (ex-info "Cannot set! immutable field"
                                        {:field (.Name field) :type (.DeclaringType field) :form (:form ast)}))
                        [(il/ldarg-0)
                         (compile val cmplrs)
                         (when value-used?
                           [(il/stloc val-local)
                            (il/ldloc val-local)])
                         (when (volatile? field)
                           (il/volatile))
                         (il/stfld field)
                         (when value-used?
                           (il/ldloc val-local))]))
                    (and (= :local (:op target))
                         (= :field (:local target))
                         (field-map (-> target :name str)))
                    (recur (assoc ast :target 
                                  {:op :instance-field
                                   :field (field-map (-> target :name str))}) 
                           cmplrs)
                    :else
                    (compile ast compilers))))
          })
        provided-methods
        (into {} (map (fn [m] [(:source-method m) (compile m specialized-compilers)]) methods))
        methods* (merge iface-methods provided-methods)]
    (reduce (fn [ctx method] (il/emit! ctx method))
            {::il/type-builder deftype-type}
            [ctor (vals methods*)]))
  (compile-getbasis deftype-type fields)
  (.CreateType deftype-type)
  [(il/ldtoken deftype-type)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defn gen-interface-compiler
  [{:keys [gen-interface-type]} compilers]
  [(il/ldtoken gen-interface-type)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defn def-compiler [{:keys [var init meta form]} compilers]
  (println "[def]" form)
  (let [var-ns (.. var Namespace Name)
        var-name (.. var Symbol)
        init-with-meta? (= :with-meta (:op init))]
    [(il/ldstr (str var-ns))
     (il/ldstr (str var-name))
     (il/call (interop/method clojure.lang.RT "var" String String))
     (when-not (nil? init)  
       [(il/dup)
        (compile init compilers)
        (convert (ast-type init) Object)
        (il/call (interop/method clojure.lang.Var "bindRoot" Object))])
     (when-not init-with-meta?
       [(il/dup)
        (compile meta compilers)
        (convert (ast-type meta) clojure.lang.IPersistentMap)
        (il/call (interop/method clojure.lang.Var "setMeta" clojure.lang.IPersistentMap))])]))

;; TODO this implementation tracks ClojureCLR's and will likely have to change
(defn import-compiler
  [{:keys [class-name]} compilers]
  [(il/call (interop/getter clojure.lang.Compiler "CurrentNamespace"))
   (il/ldstr class-name)
   (il/call (interop/method Magic.Runtime "FindType" String))
   ;; TODO throw exception if type not found
   (il/call (interop/method clojure.lang.Namespace "importClass" Type))
   ])

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
   :letfn               #'letfn-compiler
   :loop                #'loop-compiler
   :local               #'local-compiler
   :binding             #'binding-compiler
   :invoke              #'invoke-compiler
   :var                 #'var-compiler
   :the-var             #'the-var-compiler
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
   :dynamic-zero-arity  #'dynamic-zero-arity-compiler
   :static-method       #'static-method-compiler
   :instance-method     #'instance-method-compiler
   :initobj             #'initobj-compiler
   :new                 #'new-compiler
   :with-meta           #'with-meta-compiler
   :intrinsic           #'intrinsic-compiler
   :case                #'case-compiler
   :deftype             #'deftype-compiler
   :deftype-method      #'deftype-method-compiler
   :reify               #'reify-compiler
   :reify-method        #'reify-method-compiler
   :proxy               #'proxy-compiler
   :proxy-method        #'proxy-method-compiler
   :gen-interface       #'gen-interface-compiler
   :def                 #'def-compiler
   :import              #'import-compiler})

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
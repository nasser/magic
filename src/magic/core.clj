(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [magic.analyzer.util :refer [var-interfaces var-type throw!]]
            [magic.analyzer.types :as types :refer [tag ast-type ast-type-ignore-tag non-void-ast-type]]
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

(def ^:dynamic *op-stack* [])

(declare compile)
(declare compile*)
(declare base-compilers)

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

(defn reference-to-type [t]
  (when (types/is-value-type? t)
    (let [loc (il/local t)]
      [(il/stloc loc)
       (il/ldloca loc)])))

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

(defn convert-type [from to]
  (cond
    (= from :magic.analyzer.types/disregard)
    nil ; (throw (Exception. "cannot convert from disregarded type"))
    (= to :magic.analyzer.types/disregard)
    (throw (Exception. "cannot convert to disregarded type"))

    (nil? from)
    (recur Object to)

    (nil? to)
    (recur from Object)

    ;; do nothing if the types are the same
    (= from to)
    nil

    (and (types/is-enum? from) (= Object to))
    (il/box from)

    (types/is-enum? from)
    (convert-type (Enum/GetUnderlyingType from) to)

    (types/is-enum? to)
    (convert-type from (Enum/GetUnderlyingType to))

    ;; cannot convert nil to value type
    (and (nil? from) (types/is-value-type? to))
    (throw (Exception. (str "Cannot convert nil to value type " to)))

    ;; TODO truthiness
    (and (types/is-value-type? from)
         (= to Boolean))
    [(il/pop)
     (il/ldc-i4-1)]

    (and
     (= from Object)
     (= to Boolean))
    (let [isbool (il/label)
          end (il/label)]
      [(il/dup)
       (il/isinst Boolean)
       (il/brtrue isbool)
       (il/ldnull)
       (il/cgt-un)
       (il/br end)
       isbool
       (il/unbox-any Boolean)
       end])

    (= to Boolean)
    [(il/ldnull)
     (il/cgt-un)]

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

    (and (= System.Void from) (not (types/is-value-type? to)))
    (il/ldnull)

    (and (= System.Void to) (not= System.Void from))
    (il/pop)

    (and (= System.Void from) (types/is-value-type? to))
    (throw (Exception. (str "Cannot convert void to value type " to)))

    ;; use user defined implicit conversion if it exists
    (interop/method to "op_Implicit" from)
    (il/call (interop/method to "op_Implicit" from))

    ;; use user defined explicit conversion if it exists
    (interop/method to "op_Explicit" from)
    (il/call (interop/method to "op_Explicit" from))

    ;; use intrinsic conv opcodes from primitive to primitive
    (and (types/is-primitive? from) (types/is-primitive? to))
    (intrinsic-conv to)

    ;; box valuetypes to objects
    (and (types/is-value-type? from) (= to Object))
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
    (and (= from Object) (= to Byte))
    (il/call (if *unchecked-math*
               (interop/method RT "uncheckedByteCast" from)
               (interop/method RT "byteCast" from)))

    ;; unbox objects to valuetypes
    ;; TODO this will throw an exception of the object
    ;; does not have the exact runtime type of the valuetype
    ;; ie it does not perform a conversion like the above clauses
    (and (= from Object) (types/is-value-type? to))
    (il/unbox-any to)

    ;; castclass if to is a subclass of from
    (.IsSubclassOf to from)
    (il/castclass to)

    ;; do nothing if converting to super class
    (.IsSubclassOf from to)
    nil

    (and (types/is-value-type? from)
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
     ((if (types/is-value-type? from)
        il/call
        il/callvirt)
      (interop/method from "ToString"))]

    (and (not (types/is-value-type? from))
         (not (types/is-value-type? to)))
    (il/castclass to)

    :else
    (throw (Exception. (str "Cannot convert " from " to " to)))))

(defn convert [ast to]
  (when-not (:op ast)
    (throw (Exception. (str "refactor, first arg to convert needs to be an ast map, got " ast))))
  (cond
    (and (= :const (:op ast))
         (= (ast-type ast) Boolean)
         (= Object to))
    [(il/pop)
     (if (-> ast :val)
       (il/ldsfld (interop/field Magic.Constants "True"))
       (il/ldsfld (interop/field Magic.Constants "False")))]
    :else (convert-type (ast-type ast) to)))

(defmulti load-constant type)

(defn load-var [^clojure.lang.Var v]
  (let [nsname  (.. v Namespace Name ToString)
        symname (.. v Symbol ToString)]
    [(il/ldstr nsname)
     (il/ldstr symname)
     (il/call (interop/method RT "var" String String))]))

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
       (convert c Object)])
    items)))

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " k " (" (type k) ")"))

(defn load-constant-meta [k]
  (when-let [m (meta k)]
    [(convert-type (type k) clojure.lang.IObj)
     (load-constant m)
     (convert-type (type m) clojure.lang.IPersistentMap)
     (il/callvirt (interop/method clojure.lang.IObj "withMeta" clojure.lang.IPersistentMap))
     (convert-type clojure.lang.IObj (type k))]))

(defmethod load-constant Enum [v]
  (let [enum-type (Enum/GetUnderlyingType (type v))]
    (cond
      (= enum-type SByte) (load-constant (sbyte v))
      (= enum-type Int16) (load-constant (short v))
      (= enum-type Int32) (load-constant (int v))
      (= enum-type Int64) (load-constant (long v))
      (= enum-type Byte) (load-constant (byte v))
      (= enum-type UInt16) (load-constant (ushort v))
      (= enum-type UInt32) (load-constant (uint v))
      (= enum-type UInt64) (load-constant (ulong v))
      :else (throw (Exception. (str "Unsupported enum type " enum-type))))))

(defmethod load-constant clojure.lang.Var [v]
  (load-var v))

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
   (il/call (interop/method Symbol "intern" String String))
   (load-constant-meta k)])

;; NOTE the stock compiler looks up types using RT.classForName
;; if the type is not a valuetype. why? does it make a difference?
(defmethod load-constant Type [v]
  [(il/ldtoken v)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defn persistent-list-il [v]
  (let [method (interop/method clojure.lang.PersistentList "create" System.Collections.IList)]
    [(new-array (map (fn [c] [(load-constant c)
                              (convert-type (type c) Object)])
                     v))
     (il/castclass System.Collections.IList)
     (il/call method)
     (convert-type (.ReturnType method) (types/data-structure-types :seq))]))

(defmethod load-constant
  clojure.lang.ISeq [v]
  [(persistent-list-il v)
   (load-constant-meta v)])

(defmethod load-constant
  clojure.lang.APersistentVector [v]
  (let [method (interop/method clojure.lang.RT "vector" System.Object|[]|)]
    [(new-array (map (fn [c] [(load-constant c)
                              (convert-type (type c) Object)])
                     v))
     (il/call method)
     (convert-type (.ReturnType method) (types/data-structure-types :vector))]))

(defmethod load-constant
  clojure.lang.APersistentSet [v]
  (if (zero? (count v))
    (il/ldsfld (interop/field clojure.lang.PersistentHashSet "EMPTY"))
    (let [method (interop/method clojure.lang.RT "set" System.Object|[]|)]
      [(new-array (map (fn [c] [(load-constant c)
                                (convert-type (type c) Object)])
                       v))
       (il/call method)
       (convert-type (.ReturnType method) (types/data-structure-types :set))
       (load-constant-meta v)])))

(defmethod load-constant
  clojure.lang.APersistentMap [v]
  (let [method (interop/method clojure.lang.RT "mapUniqueKeys" System.Object|[]|)]
    [(->> (interleave (keys v) (vals v))
          (map (fn [k]
                 [(load-constant k)
                  (convert-type (type k) Object)]))
          new-array)
     (il/call method)
     (convert-type (.ReturnType method) (types/data-structure-types :map))
     (load-constant-meta v)]))

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
    (if (types/is-value-type? type)
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
    (if (types/is-value-type? type)
      (il/stelem type)
      (il/stelem-ref))))

(defn get-var [^clojure.lang.Var v]
  (if (or (.isDynamic v)
          (not (.isBound v)))
    (il/call (interop/method Var "deref"))
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
             (not= System.Void (ast-type ast))
             (not (types/disregard-type? ast)))
    (il/pop)))

;;; compilers

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
  (if (zero? (count items))
    (il/ldsfld (interop/field clojure.lang.PersistentVector "EMPTY"))
    (let [method (interop/method clojure.lang.RT "vector" |System.Object[]|)]
      [(prepare-array items compilers)
       (il/call method)
       (convert-type (.ReturnType method) (ast-type ast))])))

(defn set-compiler
  [{:keys [items] :as ast} compilers]
  (if (zero? (count items))
    (il/ldsfld (interop/field clojure.lang.PersistentHashSet "EMPTY"))
    (let [method (interop/method clojure.lang.RT "set" |System.Object[]|)]
      [(prepare-array items compilers)
       (il/call method)
       (convert-type (.ReturnType method) (ast-type ast))])))

(defn map-compiler
  [{:keys [keys vals] :as ast} compilers]
  (if (zero? (count keys))
    (il/ldsfld (interop/field clojure.lang.PersistentArrayMap "EMPTY"))
    (let [method (interop/method clojure.lang.RT "mapUniqueKeys" |System.Object[]|)]
      [(prepare-array (interleave keys vals) compilers)
       (il/call method)
       (convert-type (.ReturnType method) (ast-type ast))])))

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
  (let [target-type (ast-type target)]
    (if (types/is-value-type? target-type)
      [(compile (assoc target :load-address? true) compilers)
       (il/call (.GetGetMethod property))]
      [(compile target compilers)
       (il/callvirt (.GetGetMethod property))])))

(defn static-field-compiler
  "Symbolic bytecode for static fields"
  [{:keys [field]} compilers]
  (if (.IsLiteral field)
    (load-constant (.GetRawConstantValue field))
    (il/ldsfld field)))

(defn field-volatile? [f]
  (if (instance? System.Reflection.Emit.FieldBuilder f)
    (println (str "cannot look up required custom modifiers on FieldBuilder" f ", skipping"))
    (let [modifiers (into #{} (.GetRequiredCustomModifiers f))]
      (modifiers IsVolatile))))

(defn instance-field-compiler
  "Symbolic bytecode for instance fields"
  [{:keys [field target] :as ast} compilers]
  (println "[instance-field-compiler]" (:form ast) (:op target) (:local compilers))
  [(if (= target :deftype-this)
     (il/ldarg-0)
     (compile target compilers))
   (when (field-volatile? field)
     (il/volatile))
   (il/ldfld field)])

(defn dynamic-field-compiler
  "Symbolic bytecode for dynamic fields"
  [{:keys [field target]} compilers]
  [(compile target compilers)
   (load-constant (str field))
   (il/call (interop/method Magic.Dispatch "InvokeZeroArityMember" Object String))])

(defn dynamic-method-compiler
  "Symbolic bytecode for dynamic methods"
  [{:keys [op method target args]} compilers]
  (let [dispatch-method-name (if (= op :dynamic-instance-method)
                               "InvokeInstanceMethod"
                               "InvokeStaticMethod")]
    [(compile target compilers)
     (load-constant (munge (str method)))
     (prepare-array args compilers)
     (il/call (interop/method Magic.Dispatch dispatch-method-name Object String |System.Object[]|))]))

(defn dynamic-zero-arity-compiler
  "Symbolic bytecode for dynamic fields"
  [{:keys [:m-or-f target]} compilers]
  [(compile target compilers)
   (load-constant (str m-or-f))
   (il/call (interop/method Magic.Dispatch "InvokeZeroArityMember" Object String))])

(defn dynamic-constructor-compiler
  "Symbolic bytecode for dynamic constructors"
  [{:keys [type args]} compilers]
  [(load-constant type)
   (prepare-array args compilers)
   (il/call (interop/method Magic.Dispatch "InvokeConstructor" Type |System.Object[]|))
   (convert-type Object type)])

(defn static-method-compiler
  "Symbolic bytecode for static methods"
  [{:keys [method args] :as ast} compilers]
  (let [arg-types (map ast-type args)]
    [(interleave
      (map #(compile % compilers) args)
      (mapv convert
            args
            (interop/parameter-types method)))
     (il/call method)]))

(defn instance-method-compiler
  "Symbolic bytecode for instance methods"
  [{:keys [method non-virtual? target args generic-parameters] :as ast} compilers]
  (let [target-type (ast-type target)
        virtual-method? (.IsVirtual method)
        value-type-target? (types/is-value-type? target-type)]
    [(if value-type-target?
       [(compile (assoc target :load-address? true) compilers)
        (convert target target-type)]
       (compile target compilers))
     (interleave
      (mapv #(compile % compilers) args)
      (mapv convert args (interop/parameter-types method)))
     (cond
       (and (not value-type-target?) virtual-method? (not non-virtual?))
       (il/callvirt method)
       (and (not value-type-target?) (not virtual-method?))
       (il/call method)
       (and value-type-target? virtual-method?)
       (il/call method)
       (and value-type-target? (not virtual-method?))
       (il/call method)
       non-virtual?
       (il/call method)
       :else
       (il/callvirt method))]))

(defn initobj-compiler
  "Symbolic bytecode for zero arity value type constructor invocation"
  [{:keys [type load-address?]} compilers]
  (let [loc (il/local type)]
    [(il/ldloca-s loc)
     (il/initobj type)
     (if load-address?
       (il/ldloca-s loc)
       (il/ldloc loc))]))

(defn new-compiler
  "Symbolic bytecode for constructor invocation"
  [{:keys [type constructor args] :as ast} compilers]
  (let [arg-types (map ast-type args)
        ;; to get around an SRE limitation we have to bind to constructors
        ;; of TypeBuilders late
        constructor (if (instance? System.Reflection.Emit.TypeBuilder type)
                      (select-method (.GetConstructors type) arg-types)
                      constructor)]
    [(interleave
      (map #(compile % compilers) args)
      (map convert
           args
           (interop/parameter-types constructor)))
     (il/newobj constructor)]))

(defn with-meta-compiler
  "Symbolic bytecode for expressions wrapped in with-meta"
  [{:keys [expr meta] :as ast} compilers]
  [(compile expr compilers)
   (convert expr clojure.lang.IObj)
   (il/castclass clojure.lang.IObj)
   (compile meta compilers)
   (convert meta clojure.lang.IPersistentMap)
   (il/callvirt (interop/method clojure.lang.IObj "withMeta" clojure.lang.IPersistentMap))])

(defn loop-compiler
  [{:keys [bindings body] :as ast} compilers]
  (let [;; uniqued names -> il/locals
        binding-map
        (reduce (fn [m binding]
                  (assoc m
                         (-> binding :name)
                         (il/local (non-void-ast-type binding)
                                   (str (-> binding :name)))))
                {}
                bindings)
        binding-vector (mapv #(binding-map (-> % :name)) bindings)
        recur-target (il/label)
        ;; TODO compiler local and recur with compilers or cmplrs?
        specialized-compilers
        (merge compilers
               {:local (fn loop-local-compiler
                         [{:keys            [name init load-address?]
                           :as              ast} cmplrs]
                         (if-let [loc (-> name binding-map)]
                           (if load-address?
                             (il/ldloca loc)
                             [(il/ldloc loc)
                              #_(convert (ast-type init) (ast-type ast))])
                           (compile* ast compilers)))}
               {:recur (fn loop-recur-compiler
                         [{:keys [exprs]
                           :as   ast} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert %1 (non-void-ast-type %2)) exprs bindings))
                          (map il/stloc (reverse binding-vector))
                          (il/br recur-target)])})]
    ;; emit local initializations
    [(map (fn [binding]
            [(compile binding specialized-compilers)
             (convert (:init binding) (non-void-ast-type binding))
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
                  [{:keys [name form load-address?] {:keys [locals]} :env :as ast} cmplrs]
                  (if-let [loc (-> name binding-map)]
                    (if load-address?
                      (il/ldloca loc)
                      [(il/ldloc loc)
                       (when (-> form locals :init)
                         ;; non-void-ast-type here because the conversion from 
                         ;; void would have already happened in the bindings
                         (convert-type (non-void-ast-type (-> form locals :init)) (non-void-ast-type ast)))])
                    (compile* ast compilers)))})]
    ;; emit local initializations
    [(map (fn [binding]
            [(compile binding specialized-compilers)
             (convert (:init binding) (non-void-ast-type binding))
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
                  [{:keys [name form load-address?] {:keys [locals]} :env :as ast} cmplrs]
                  (if-let [loc (-> name binding-map)]
                    (if load-address?
                      (il/ldloca loc)
                      [(il/ldloc loc)
                       (when (-> form locals :init)
                         (convert (-> form locals :init) (non-void-ast-type ast)))])
                    (compile* ast compilers)))})
        binding-il (map #(compile % specialized-compilers) bindings)]
    ;; emit local initializations
    [(map (fn [il binding]
            [(drop-last il)
             (convert (:init binding) (non-void-ast-type binding))
             (il/stloc (binding-map (:name binding)))])
          binding-il bindings)
     (interleave
      (map (fn [il binding]
             [(il/ldloc (binding-map (:name binding)))
              (when-let [fn-type (-> binding :init :fn-type)]
                (convert-type (non-void-ast-type binding) fn-type))
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
                 (convert test Boolean)
                 (il/brtrue then-label)
                 (compile else compilers)
                 (when-not (types/disregard-type? else)
                   (convert else if-expr-type))
                 (il/br end-label)
                 then-label
                 (compile then compilers)
                 (when-not (types/disregard-type? then)
                   (convert then if-expr-type))
                 end-label])))

(defn binding-compiler
  [{:keys [init] :as ast} compilers]
  (compile init compilers))

(defn local-compiler
  [{:keys [name load-address? arg-id local] :as ast} compilers]
  (case local
    :arg
    (let [type (ast-type ast)
          type-ignore-tag (ast-type-ignore-tag ast)]
      (cond
        ;; we dont generate reference bytecode here, that happens elsewhere
        ;; the result is somewhat messy but could be cleaned up in a peephole pass
        load-address?
        (load-argument-address arg-id)
        :else
        [(load-argument-standard arg-id)]))
    :fn
    [(load-argument-standard 0)]
    :proxy-this
    (load-argument-standard 0)
    (throw! "Local " name " not bound, could not compile! " local)))

(defn implementing-interface [t bm]
  (->> (.GetInterfaces t)
       (filter #(= "Magic" (.Namespace %)))
       (filter
        #(contains?
          (set (.. t (GetInterfaceMap %) TargetMethods))
          bm))
       first))

(def ifn-invoke-methods
  (mapv #(apply interop/method IFn "invoke" (repeat % Object)) (range 21)))

(def variadic-ifn-invoke-method
  (apply interop/method IFn "invoke" (concat (repeat 20 Object) [System.Object|[]|])))

(defn ifn-invoke-compiler [{:keys [args] :as ast} compilers]
  (let [positional-args (take 20 args)
        rest-args (drop 20 args)
        invoke-method
        (if (empty? rest-args)
          (ifn-invoke-methods (count args))
          variadic-ifn-invoke-method)]
    [(il/castclass IFn)
     (interleave
      (map #(compile % compilers) positional-args)
      (map #(convert % Object) positional-args))
     (when-not (empty? rest-args)
       [(load-constant (count rest-args))
        (il/newarr Object)
        (map-indexed
         (fn [i arg]
           [(il/dup)
            (load-constant (int i))
            (compile arg compilers)
            (convert arg Object)
            (il/stelem-ref)])
         rest-args)])
     (il/callvirt invoke-method)
     (convert-type Object (ast-type ast))]))

(defn invoke-compiler
  [{:keys [fn args] :as ast} compilers]
  ;; TODO revisit high performance generic function interfaces
  (let [;; fn-type (var-type fn)
        ;; arg-types (map ast-type args)
        ;; TODO this is hacky and gross
        ;; best-method (when fn-type
        ;;               (select-method (filter #(= (.Name %) "invokeTyped")
        ;;                                      (.GetMethods fn-type))
        ;;                              arg-types))
        ;; param-types (when best-method
        ;;               (map #(.ParameterType %) (.GetParameters best-method)))
        ;; interface-match (when best-method
        ;;                   (implementing-interface fn-type best-method))
        ]
    [(compile fn compilers)
     (ifn-invoke-compiler ast compilers)
     #_(if interface-match
         [(il/castclass interface-match)
          (interleave
           (map #(compile % compilers) args)
           (map #(convert %1 %2) arg-types param-types))
          (il/callvirt (apply interop/method interface-match "invokeTyped" param-types))
          (convert (.ReturnType best-method) (ast-type ast))]
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
          [(compile target' compilers)
           (compile val compilers)
           (when value-used?
             [(il/stloc v)
              (il/ldloc v)])
           (convert val (.FieldType field))
           (when (field-volatile? field)
             (il/volatile))
           (il/stfld field)
           (when value-used?
             (il/ldloc v))]))
      (= target-op :instance-property)
      (let [v (il/local (ast-type val))]
        [(compile target' compilers)
         (compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert val (.PropertyType property))
         (il/callvirt (.GetSetMethod property))
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-field)
      (let [v (il/local (ast-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert val (.FieldType field))
         (il/stsfld field)
         (if value-used?
           (il/ldloc v))])
      (= target-op :static-property)
      (let [v (il/local (ast-type val))]
        [(compile val compilers)
         (if value-used?
           [(il/stloc v)
            (il/ldloc v)])
         (convert val (.PropertyType property))
         (il/call (.GetSetMethod property))
         (when value-used?
           (il/ldloc v))])
      (= target-op :dynamic-zero-arity)
      [(compile target' compilers)
       (load-constant (str (-> target :m-or-f)))
       (compile val compilers)
       (convert val Object)
       (il/call (magic.interop/method Magic.Dispatch "SetMember" Object String Object))
       (convert-type Object (ast-type val))
       (when-not value-used?
         (il/pop))]
      (= target-op :var)
      (if (:assignable? target)
        (let [v (il/local (ast-type val))]
          [(load-var (:var target))
           (compile val compilers)
           (when value-used?
             [(il/stloc v)
              (il/ldloc v)])
           (convert val Object)
           (il/call (interop/method clojure.lang.Var "set" Object))
           (il/pop)
           (when value-used?
             (il/ldloc v))])
        (throw! "Cannot assign to non-assignable var " (:var target))))))

(defn has-arity-method
  "Symbolic bytecode for the IFnArity.HasArity method"
  [fixed-arities variadic-arity]
  (il/method
   "HasArity"
   (enum-or MethodAttributes/Public
            MethodAttributes/Virtual)
   Boolean [(il/parameter Int32)]
   (let [ret-true (il/label)]
     [(when variadic-arity
        [(il/ldarg-1)
         (load-constant (int variadic-arity))
         (il/bge ret-true)])
      (map (fn [arity]
             [(il/ldarg-1)
              (load-constant (int arity))
              (il/beq ret-true)])
           fixed-arities)
      (il/ldc-i4-0)
      (il/ret)
      ret-true
      (il/ldc-i4-1)
      (il/ret)])))

(defn get-required-arity-method
  "Symbolic bytecode for the RestFn.getRequiredArity method"
  [variadic-arity]
  (when variadic-arity
    (il/method
     "getRequiredArity"
     (enum-or MethodAttributes/Public
              MethodAttributes/Virtual)
     Int32 []
     [(load-constant (int variadic-arity))
      (il/ret)])))

(defn private-constructor [t]
  (first (.GetConstructors t (enum-or BindingFlags/NonPublic BindingFlags/Instance))))

;; TODO this is unused
(def default-constructor
  (il/constructor
   (enum-or MethodAttributes/Public)
   CallingConventions/Standard
   []
   [(il/ldarg-0)
    (il/call (private-constructor clojure.lang.AFunction))
    (il/ret)]))

(defn ifn-type? [t]
  (.IsAssignableFrom clojure.lang.IFn t))

(defn compile-fn-type [{:keys [methods variadic? closed-overs fn-type fn-type-cctor local] :as ast} compilers]
  (when-not (.IsCreated fn-type)
    (let [fixed-arity-methods (remove :variadic? methods)
          fn-name-tag (-> local :meta :tag types/resolve)
          signatures (->> fixed-arity-methods
                          (map (fn [method]
                                 (let [return-type (or (-> method :form first meta :tag types/resolve)
                                                       fn-name-tag
                                                       (-> method :body ast-type))
                                       param-types (map non-void-ast-type (:params method))]
                                   (list* return-type param-types))))
                          (remove (fn [sig] (some #(not (instance? Type %)) sig))) ;; remove signatures with non-type elemnts (eg disregarded type keyword)
                          (remove (fn [sig] (every? #(= Object %) sig))) ;; remove signatures that are all Object
                          (remove (fn [sig] (some ifn-type? sig))) ;; remove signatures with function types
                          (remove (fn [sig] (some #(= System.Void %) sig)))) ;; remove signatures with void returns 
          interfaces (->> (map #(interop/generic-type "Magic.Function" %) signatures)
                          (remove nil?)
                          (into #{}))
          fixed-arities (->> methods
                             (remove :variadic?)
                             (map :fixed-arity))
          variadic-arity (when variadic?
                           (->> methods
                                (filter :variadic?)
                                first
                                :fixed-arity))
          closed-over-field-map
          (reduce-kv
           (fn [m k v]
             (assoc m k (il/unique (il/field (ast-type v) (str (:form v))
                                             FieldAttributes/Assembly))))
           {}
           closed-overs)
          closed-over-fields (-> closed-over-field-map vals vec)
          specialized-compilers
          (merge
           compilers
           {:local
            (fn fn-local-compiler
              [{:keys [name load-address?] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)
                 ;; it would be nice if we could ldflda here, but 
                 ;; our closure fields are read-only and the clr does
                 ;; not allow ldflda of read-only fields
                 (when load-address?
                   (reference-to-type (ast-type ast)))]
                (local-compiler ast _cmplrs)))})
          ctor (il/constructor
                (enum-or MethodAttributes/Public)
                CallingConventions/Standard []
                [(il/ldarg-0)
                 (il/call (private-constructor
                           (if variadic?
                             clojure.lang.RestFn
                             clojure.lang.AFunction)))
                 (il/ret)])
          methods* (->> methods
                        (map #(assoc % :fn-name-tag fn-name-tag))
                        (map #(compile % specialized-compilers)))]
      (reduce (fn [ctx x] (il/emit! ctx x))
              {::il/type-builder fn-type}
              [closed-over-fields ctor methods* (has-arity-method fixed-arities variadic-arity) (get-required-arity-method variadic-arity)])
      (when fn-type-cctor
        (il/emit! {::il/ilg (.GetILGenerator fn-type-cctor)} (il/ret)))
      (doseq [i interfaces]
        (.AddInterfaceImplementation fn-type i)))
    (when fn-type-cctor
      (.. fn-type-cctor GetILGenerator (Emit OpCodes/Ret)))
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
  [{:keys [fn-name-tag body params form variadic?]} compilers]
  (let [param-hint (-> form first tag)
        param-types (mapv ast-type params)
        param-names (into #{} (map :name params))
        obj-params (mapv (constantly Object) params)
        param-il (map #(il/parameter (ast-type %) (-> % :form str)) params)
        param-il-unhinted (map #(il/parameter Object (-> % :form str)) params)
        body-type (if (types/disregard-type? body)
                    System.Void
                    (ast-type body))
        return-type (or param-hint fn-name-tag body-type)
        public-virtual (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        recur-target (il/label)
        invoke-method-name (if variadic? "doInvoke" "invoke")
        specialized-compilers
        (merge compilers
               {:recur (fn fn-recur-compiler
                         [{:keys [exprs]
                           :as   ast} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert %1 %2) exprs param-types))
                          (map store-argument (->> params count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn fn-method-local-compiler
                         [{:keys [name local] :as ast} local-compilers]
                         (if (and (= local :arg)
                                  (param-names name))
                           (local-compiler (update ast :arg-id inc) local-compilers)
                           (compile* ast compilers)))})
        compiled-body
        (compile body specialized-compilers)
        unhinted-method
        (il/method
         invoke-method-name
         public-virtual
         Object param-il-unhinted
         [recur-target
          compiled-body
          (convert-type return-type Object)
          (il/ret)])
        hinted-method
        (il/method
         "invokeTyped"
         public-virtual
         return-type param-il
         [recur-target
          compiled-body
          (convert-type body-type return-type)
          (il/ret)])
        unhinted-shim
        (il/method
         invoke-method-name
         public-virtual
         Object param-il-unhinted
         [(il/ldarg-0)
          (interleave
           (map (comp load-argument-standard inc) (range))
           (map #(convert-type Object %) param-types))
          (il/callvirt hinted-method)
          (convert-type return-type Object)
          (il/ret)])]
    [(if (and (= param-types obj-params)
              (= return-type Object))
       unhinted-method
       [hinted-method unhinted-shim])]))

(defn try-compiler
  [{:keys [catches closed-overs body finally top-level outside-type?] :as ast {:keys [empty-stack?]} :env} compilers]
  (if (and (empty? catches)
           (nil? finally))
    (compile body compilers)
    (let [expr-type (ast-type ast)
          expr-has-value?
          (not (or (types/disregard-type? ast)
                   (= expr-type System.Void)))
          method-attributes
          (if outside-type?
            (enum-or MethodAttributes/Assembly MethodAttributes/Static)
            (enum-or MethodAttributes/Assembly))
          closed-overs-map
          (into {} (map (fn [name i] [name i]) (keys closed-overs) (range)))
          closure-compilers
          (merge compilers
                 {:local (fn try-local-compiler
                           [{:keys [name] :as ast} _compilers]
                           (if-let [arg-id (closed-overs-map name)]
                             (load-argument-standard (if outside-type? arg-id (inc arg-id)))
                             (compile* ast compilers)))})
          bodyfn
          (fn [compilers]
            (let [try-local (when expr-has-value?
                              (il/local expr-type))]
              [(il/exception
                [(if-not expr-has-value?
                   [(compile body compilers)
                    (map #(compile % compilers) catches)]
                   [(compile body compilers)
                    (convert body expr-type)
                    (when expr-has-value?
                      (il/stloc try-local))
                    (interleave
                     (map #(compile % compilers) catches)
                     (map #(when-not (types/disregard-type? %) [(convert % expr-type) (il/stloc try-local)]) catches))])
                 [(when finally
                    (il/finally
                      (compile finally compilers)))]])
               (cond
                 expr-has-value? (il/ldloc try-local)
                 (not (= System.Void expr-type)) (il/ldnull))]))
          method-return-type (if (types/disregard-type? ast)
                               System.Void
                               expr-type)
          method-il (il/method
                     (str (gensym "try"))
                     method-attributes
                     method-return-type (->> closed-overs vals (mapv non-void-ast-type))
                     [(bodyfn closure-compilers)
                      (il/ret)])]
      (if (or top-level empty-stack?)
        ;; at the top level or when the stack is known to be empty we can
        ;; emit a normal try statement inline
        (bodyfn compilers)
        ;; otherwise we lift the try into a method closure.

        [(when-not outside-type?
           ;; if we're inside of a type the try method is an instance method
           ;; and we emit `this` to invoke it
           (load-argument-standard 0))
         (->> closed-overs vals (map #(compile % compilers)))
         (il/call method-il)
         (when (types/disregard-type? ast)
           (il/ldnull))]))))

(defn throw-compiler
  [{:keys [exception] {:keys [context]} :env} compilers]
  (when-not exception
    (throw (Exception. "throw must take an argument outside of a catch")))
  [(compile exception compilers)
   (convert exception Exception)
   (il/throw)])

(defn catch-compiler
  [{:keys [class local body]} compilers]
  (let [catch-local-name (:name local)
        catch-local (il/local (-> class :val))
        compilers+local
        (merge compilers
               {:local
                (fn catch-local-compiler
                  [{:keys [name load-address?] :as ast} cmplrs]
                  (if (= name catch-local-name)
                    (if load-address?
                      (il/ldloca catch-local)
                      (il/ldloc catch-local))
                    (compile* ast compilers)))})
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
   (convert target Object)
   (il/call (interop/method System.Threading.Monitor "Enter" Object))])

(defn monitor-exit-compiler
  [{:keys [target]} compilers]
  [(compile target compilers)
   (convert target Object)
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
              (convert local Int32)]
             (= Char local-type)
             (il/br default-label2)
             :else
             [(compile local compilers)
              (convert local Object)
              (il/call (interop/method clojure.lang.Util "IsNonCharNumeric" Object))
              (il/brfalse default-label2)
              (compile local compilers)
              (convert local Object)
              (il/call (interop/method clojure.lang.Util "ConvertToInt" Object))])]
          (:hash-equiv :hash-identity)
          [(compile local compilers)
           (convert local Object)
           (il/call (interop/method clojure.lang.Util "hash" Object))])
        switch-body-il
        (case mode
          :int
          (map
           (fn [label test expression next-label]
             [label
              (when-not (= local-type Int32)
                (let [equiv-method (or (interop/method clojure.lang.Util "equiv" local-type (or (ast-type test) Object))
                                       (interop/method clojure.lang.Util "equiv" Object Object))
                      parameter-types (->> equiv-method .GetParameters (map #(.ParameterType %)))]
                  [(compile local compilers)
                   (convert local (first parameter-types))
                   (compile test compilers)
                   (convert test (last parameter-types))
                   (il/call equiv-method)
                   (il/brfalse next-label)]))
              (compile expression compilers)
              (convert expression expr-type)
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
                 (compile test compilers)
                 (il/ceq)
                 (il/brfalse next-label)])
              (compile expression compilers)
              (convert expression expr-type)
              (il/br return-label)])
           labels
           tests
           expressions
           switch-values
           (if sparse? next-labels (repeat default-label2)))
          :hash-equiv
          (map
           (fn [label test expression switch-value next-label]
             (let [equiv-method (or (interop/method clojure.lang.Util "equiv" local-type (or (ast-type test) Object))
                                    (interop/method clojure.lang.Util "equiv" Object Object))
                   parameter-types (->> equiv-method .GetParameters (map #(.ParameterType %)))]
               [label
                (when-not (skip-check switch-value)
                  [(compile local compilers)
                   (convert local (first parameter-types))
                   (compile test compilers)
                   (convert test (last parameter-types))
                   (il/call equiv-method)
                   (il/brfalse next-label)])
                (compile expression compilers)
                (convert expression expr-type)
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
     (convert default expr-type)
     return-label]))

(defn default-override-method [method attributes]
  (il/method
   (.Name method)
   attributes
   (.ReturnType method)
   (mapv #(.ParameterType %) (.GetParameters method))
   [(il/newobj (interop/constructor NotImplementedException))
    (il/throw)]))

(defn all-abstract-methods [type]
  (->> type
       .GetProperties
       (mapcat #(vector (.GetSetMethod %) (.GetGetMethod %)))
       (concat (.GetMethods type))
       (remove nil?)
       (filter #(.IsAbstract %))))

(defn compile-proxy-type [{:keys [args super interfaces closed-overs fns proxy-type] :as ast} compilers]
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
           compilers
           {:local
            (fn proxy-local-compiler
              [{:keys [name load-address?] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)
                 ;; it would be nice if we could ldflda here, but 
                 ;; our closure fields are read-only and the clr does
                 ;; not allow ldflda of read-only fields
                 (when load-address?
                   (reference-to-type (ast-type ast)))]
                (compile* ast compilers)))})
          provided-methods
          (into {} (map (fn [f] [(:source-method f) (compile f specialized-compilers)]) fns))
          methods (merge iface-methods abstract-methods provided-methods)
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
                  (map convert args (interop/parameter-types super-ctor)))
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
                      (vals methods))]
      (il/emit! ctx ctor)
      (.CreateType proxy-type))))

(defn proxy-compiler [{:keys [class-and-interface proxy-type args closed-overs form] :as ast} compilers]
  (compile-proxy-type ast compilers)
  (when-not (every? #(and (= :const (:op %))
                          (= :class (:type %)))
                    class-and-interface)
    (throw (ex-info (str "Classes and interfaces passed to proxy must be type constants")
                    {:form form})))
  [(map #(compile % compilers) args)
   (map #(compile % compilers) (vals closed-overs))
   (il/newobj (first (.GetConstructors proxy-type)))])

(defn method-compiler
  "Shared compiler for methods of proxy, reify, and deftype"
  [{:keys [op name body source-method reify-type deftype-type] :as ast} compilers]
  (let [proxy? (= op :proxy-method)
        name (str name)
        explicit-override? (string/includes? name ".")
        super-override (enum-or MethodAttributes/Public MethodAttributes/Virtual)
        explicit-override (enum-or MethodAttributes/Private MethodAttributes/Virtual MethodAttributes/NewSlot)
        iface-override (enum-or super-override MethodAttributes/Final MethodAttributes/NewSlot)
        this-type (or reify-type deftype-type)
        param-types (mapv #(.ParameterType %) (.GetParameters source-method))
        param-type-bindings (if proxy? param-types (vec (concat [this-type] param-types)))
        param-names (into #{} (map :name (:params ast)))
        return-type (.ReturnType source-method)
        attributes (if (.. source-method DeclaringType IsInterface)
                     (if explicit-override? explicit-override iface-override)
                     super-override)
        body-type (ast-type body)
        recur-target (il/label)
        specialized-compilers
        (merge compilers
               {:recur (fn method-recur-compiler
                         [{:keys [exprs]} cmplrs]
                         [(interleave
                           (map #(compile % cmplrs) exprs)
                           (map #(convert %1 %2) exprs param-types))
                          (map store-argument (->> param-types count inc (range 1) reverse))
                          (il/br recur-target)])
                :local (fn method-local-compiler
                         [{:keys [arg-id name local load-address?] :as ast} _cmplrs]
                         (if (and (= local :arg)
                                  (param-names name))
                           (let [arg-offset (if proxy? (inc arg-id) arg-id)]
                             (if load-address?
                               (load-argument-address arg-offset)
                               [(load-argument-standard arg-offset)
                                (convert-type (param-type-bindings arg-id) (ast-type ast))]))
                           (compile* ast compilers)))})]
    (il/method
     name
     attributes
     return-type param-types
     (when explicit-override? source-method)
     [recur-target
      (compile body specialized-compilers)
      (convert body return-type)
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
           {}
           closed-overs)
          closed-over-fields (-> closed-over-field-map vals vec)
          specialized-compilers
          (merge
           compilers
           {:local
            (fn proxy-local-compiler
              [{:keys [name load-address?] :as ast} _cmplrs]
              (if-let [fld (closed-over-field-map name)]
                [(il/ldarg-0)
                 (il/ldfld fld)
                 ;; it would be nice if we could ldflda here, but 
                 ;; our closure fields are read-only and the clr does
                 ;; not allow ldflda of read-only fields
                 (when load-address?
                   (reference-to-type (ast-type ast)))]
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
              [closed-over-fields ctor meta-ctor (vals methods*)
               (iobj-implementation meta-il meta-field meta-ctor closed-over-field-map)]))
    (.CreateType reify-type)))

(defn reify-compiler [{:keys [closed-overs reify-type meta] :as ast} compilers]
  (compile-reify-type ast compilers)
  (if (:constant? meta)
    [(map #(compile % compilers) (vals closed-overs))
     (il/newobj (first (.GetConstructors reify-type)))]
    [(compile meta compilers)
     (map #(compile % compilers) (vals closed-overs))
     (il/newobj (last (.GetConstructors reify-type)))]))

(defn compile-deftype-getbasis [ctx tb symbols]
  (let [ilg (->> tb
                 .GetMethods
                 (filter #(= "getBasis" (.Name %)))
                 first
                 .GetILGenerator)]
    (il/emit!
     {::il/ilg ilg}
     [(load-constant symbols)
      (il/ret)])))

(defn compile-defrecord-create [ctx tb symbols ctor]
  (let [keywords (mapv keyword symbols)
        ilg (->> tb
                 .GetMethods
                 (filter #(= "create" (.Name %)))
                 first
                 .GetILGenerator)]
    (il/emit!
     (merge ctx {::il/ilg ilg})
     (let [kw-local (il/local clojure.lang.Keyword)]
       [(map
         (fn [kw]
           [(load-constant kw)
            (il/stloc kw-local)
            (il/ldarg-0)
            (il/ldloc kw-local)
            (il/ldnull)
            (il/callvirt (interop/method clojure.lang.ILookup "valAt" Object Object))
            (il/ldarg-0)
            (il/ldloc kw-local)
            (il/callvirt (interop/method clojure.lang.IPersistentMap "without" Object))
            (il/starg-s (byte 0))])
         keywords)
        (il/ldnull)
        (il/ldarg-0)
        (il/call (interop/method clojure.lang.RT "seqOrElse" Object))
        (load-constant (int 0))
        (load-constant (int 0))
        (il/newobj ctor)
        (il/ret)]))))

(defn deftype-compiler [{:keys [fields methods positional-factory implements name classname deftype-type]} compilers]
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
        defrecord? (.IsAssignableFrom clojure.lang.IRecord deftype-type)
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
        ctors
        (merge
         {(count ctor-params) ctor}
         ;; defrecord gets extra constructors that invoke the main ctor
         (when defrecord?
           {(- (count ctor-params) 2)
            (il/constructor
             MethodAttributes/Public
             CallingConventions/Standard
             (drop-last 2 ctor-params)
             [(il/ldarg-0)
              (map #(load-argument-standard (inc %))
                   (range (- (count ctor-params) 2)))
              (load-constant (int 0))
              (load-constant (int 0))
              (il/call ctor)
              (il/ret)])
            (- (count ctor-params) 4)
            (il/constructor
             MethodAttributes/Public
             CallingConventions/Standard
             (drop-last 4 ctor-params)
             [(il/ldarg-0)
              (map #(load-argument-standard (inc %))
                   (range (- (count ctor-params) 4)))
              (il/ldnull)
              (il/ldnull)
              (load-constant (int 0))
              (load-constant (int 0))
              (il/call ctor)
              (il/ret)])}))
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
              (let [arg-count (count args)]
                [(interleave
                  (map #(compile % local-compilers) args)
                  (map convert args ctor-params))
                 (il/newobj (ctors arg-count))])
              (compile ast (assoc local-compilers
                                  :new (:new compilers)))))
          :local
          (fn deftype-local-compiler
            [{:keys [local name] :as ast} inner-compilers]
            (if (and (= :field local)
                     (field-map (str name)))
              (compile {:op :instance-field
                        :target :deftype-this
                        :field (field-map (str name))}
                       inner-compilers)
              (compile* ast compilers)))
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
                    (compile ast compilers))))})
        provided-methods
        (into {} (mapv (fn [m] [(:source-method m) (compile m specialized-compilers)]) methods))
        methods* (merge iface-methods provided-methods)
        ctx' (reduce (fn [ctx method] (il/emit! ctx method))
                     {::il/type-builder deftype-type}
                     [(vals ctors) (vals methods*)])]
    (compile-deftype-getbasis ctx' deftype-type
                              (if defrecord?
                                (vec (drop-last 4 fields))
                                fields))
    (when defrecord?
      (compile-defrecord-create ctx' deftype-type (drop-last 4 fields) ctor))
    (.CreateType deftype-type)
    [(when-not defrecord?
       [(compile positional-factory compilers)
        (il/pop)])
     (il/ldtoken deftype-type)
     (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))]))

(defn gen-interface-compiler
  [{:keys [gen-interface-type]} compilers]
  [(il/ldtoken gen-interface-type)
   (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defn def-compiler [{:keys [var init meta]} compilers]
  (let [var-ns (.. var Namespace Name)
        var-name (.. var Symbol)
        [init meta] (if (= :with-meta (:op init))
                      [(:expr init) meta]
                      [init meta])]
    [(compile {:op :the-var :var var} compilers)
     (when-not (nil? init)
       [(il/dup)
        (compile init compilers)
        (convert init Object)
        (il/call (interop/method clojure.lang.Var "bindRoot" Object))])
     (when-not (nil? meta)
       [(il/dup)
        (compile meta compilers)
        (convert meta clojure.lang.IPersistentMap)
        (il/call (interop/method clojure.lang.Var "setMeta" clojure.lang.IPersistentMap))])
     (when (-> meta :form :dynamic)
       (il/call (interop/method clojure.lang.Var "setDynamic")))]))

;; TODO this implementation tracks ClojureCLR's and will likely have to change
(defn import-compiler
  [{:keys [class-name]} compilers]
  (let [import-class-label (il/label)]
    [(il/call (interop/getter clojure.lang.Compiler "CurrentNamespace"))
     (if-let [t (types/resolve class-name)]
       [(il/ldtoken t)
        (il/call (interop/method Type "GetTypeFromHandle" RuntimeTypeHandle))]
       ;; TODO should this be an error? just throw the exception here?
       [(il/ldstr class-name)
        (il/call (interop/method Magic.Runtime "FindType" String))
        (il/dup)
        (il/ldnull)
        (il/ceq)
        (il/brfalse import-class-label)
        (il/ldstr (str "Could not find type " class-name " during import"))
        (il/newobj (interop/constructor InvalidOperationException String))
        (il/throw)])
     import-class-label
     (il/call (interop/method clojure.lang.Namespace "importClass" Type))]))

(defn tagged-compiler
  [{:keys [expr tag]} compilers]
  [(compile expr compilers)
   (convert expr (types/resolve tag))])

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
   :dynamic-method      #'dynamic-method-compiler
   :dynamic-instance-method #'dynamic-method-compiler
   :dynamic-static-method   #'dynamic-method-compiler
   :dynamic-zero-arity  #'dynamic-zero-arity-compiler
   :dynamic-constructor #'dynamic-constructor-compiler
   :static-method       #'static-method-compiler
   :instance-method     #'instance-method-compiler
   :initobj             #'initobj-compiler
   :new                 #'new-compiler
   :with-meta           #'with-meta-compiler
   :intrinsic           #'intrinsic-compiler
   :case                #'case-compiler
   :deftype             #'deftype-compiler
   :deftype-method      #'method-compiler
   :reify               #'reify-compiler
   :reify-method        #'method-compiler
   :proxy               #'proxy-compiler
   :proxy-method        #'method-compiler
   :gen-interface       #'gen-interface-compiler
   :def                 #'def-compiler
   :import              #'import-compiler
   :tagged              #'tagged-compiler})

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


(defn compile*
  ([ast]
   (compile* ast (get-compilers)))
  ([ast compilers]
   (when-let [compiler (ast->compiler ast compilers)]
     (binding [*op-stack* (conj *op-stack* (:op ast))]
       (try
         (compiler ast compilers)
         (catch Exception e
           (throw (Exception. (str "Failed to compile " (:form ast) " " *op-stack*) e))))))))

(defn maybe-compile-reference-to [{:keys [op load-address?] :as ast}]
  ;; locals will have compiled to ldloca already, so we skip them here
  (when (and (not= :local op)
             load-address?)
    (reference-to-type (ast-type ast))))

(defn compile
  "Generate symbolic bytecode for AST node"
  ([ast]
   (compile ast (get-compilers)))
  ([ast compilers]
   [(compile* ast compilers)
    (maybe-compile-reference-to ast)]))
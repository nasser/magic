;; this is magic
(ns magic.core
  (:refer-clojure :exclude [compile resolve])
  (:require [mage.core :as il]
            [magic.analyzer :as ana]
            [clojure.string :as string])
  (:import [clojure.lang RT Numbers Compiler LineNumberingTextReader
            Symbol Namespace IFn Var Keyword Symbol
            IPersistentList LazySeq IPersistentVector IPersistentMap IPersistentSet
            PersistentArrayMap PersistentHashSet PersistentList PersistentVector]
           [clojure.lang.CljCompiler.Ast RHC ParserContext
            Expr LiteralExpr StaticMethodExpr InstanceMethodExpr StaticPropertyExpr NumberExpr
            InstancePropertyExpr InstanceFieldExpr MapExpr VarExpr TheVarExpr InvokeExpr HostExpr
            FnExpr FnMethod BodyExpr LocalBindingExpr IfExpr VectorExpr NewExpr LetExpr CaseExpr
            MonitorEnterExpr MonitorExitExpr InstanceZeroArityCallExpr StaticFieldExpr InstanceOfExpr
            ThrowExpr TryExpr TryExpr+CatchClause UnresolvedVarExpr EmptyExpr SetExpr ImportExpr RecurExpr
            KeywordInvokeExpr KeywordExpr NilExpr StringExpr]
           [System.IO FileInfo Path]
           [System.Threading Monitor]
           [System.Reflection TypeAttributes MethodAttributes FieldAttributes FieldInfo MethodInfo PropertyInfo]
           AppDomain
           System.Reflection.Emit.OpCodes
           AssemblyName
           AssemblyBuilderAccess))

(defn field-map
  "Get a map of all of an object's fields. Reflects."
  [obj]
  (-> obj
      .GetType
      (.GetFields (enum-or BindingFlags/Instance BindingFlags/NonPublic BindingFlags/Public))
      (->> (mapcat #(vector (keyword (.Name %))
                            (.GetValue % obj)))
           (apply hash-map))))

(defn property-map
  "Get a map of all of an object's properties. Reflects."
  [obj]
  (-> obj
      .GetType
      (.GetProperties (enum-or BindingFlags/Instance BindingFlags/NonPublic BindingFlags/Public))
      (->> (mapcat #(vector (keyword (.Name %))
                            (try (.GetValue % obj nil)
                              (catch Exception e nil))))
           (apply hash-map))))

(defn data-map
  "Get a map of all of an object's fields and properties. Reflects."
  [obj]
  (cond
    (nil? obj) obj
    (.IsValueType (type obj)) obj
    (instance? System.Collections.IEnumerable obj) (map data-map obj)
    :else (merge {::type (type obj)}
                 (field-map obj)
                 (property-map obj))))

(defn analyze
  ([form] (analyze form RHC/Expression))
  ([form rhc] (Compiler/Analyze (ParserContext. rhc) form)))

(defn find-method
  ([type name & params] (.GetMethod type name (into-array Type params))))

(defn find-constructor
  ([type & params] (.GetConstructor type (into-array Type params))))

(defn property-getter [type name]
  (find-method type (str "get_" name)))

(defn property-setter [type name]
  (find-method type (str "set_" name)))

(def genstr (comp str gensym str))

(defn find-file [ns]
  (-> ns
      name
      (string/replace "." "/")
      (str ".clj")
      RT/FindFile
      ))

;; http://stackoverflow.com/questions/24922478/is-there-a-way-to-read-all-the-forms-in-a-clojure-file
(defn read-all
  [file]
  (let [rdr (PushbackTextReader. (.OpenText file))]
    (loop [forms []]
      (let [form (try (read rdr) (catch Exception e nil))]
        (if form
          (recur (conj forms form))
          (do (.Close rdr)
            forms))))))

(defn append [a col]
  (concat col [a]))

(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))

(defn load-argument [i]
  (cond
    (= i 0) (il/ldarg-0)
    (= i 1) (il/ldarg-1)
    (= i 2) (il/ldarg-2)
    (= i 3) (il/ldarg-3)
    :else (il/ldarg i)))

(declare clr-type)

;; TODO repeated shape between instance-zero-arity-call-type instance-zero-arity-call-symbolizer
(defn instance-zero-arity-call-type [izac]
  (let [{:keys [_target _memberName]} (data-map izac)
        typ (clr-type _target)]
    (if-let [info (.GetField typ _memberName)]
      (.FieldType info)
      (if-let [info (.GetProperty typ _memberName)]
        (.PropertyType info)
        (if-let [info (find-method typ _memberName)]
          (.ReturnType info)
          (throw (Exception. (str "hell " typ _memberName)))))))) ;; TODO throw exception here? what does it even mean?!

(defn zero-arity-type [class name]
  (or (if-let [info (.GetField class name)]
        (.FieldType info))
      (if-let [info (.GetProperty class name)]
        (.PropertyType info))
      (if-let [info (.GetMethod class name Type/EmptyTypes)]
        (.ReturnType info))))

(defn zero-arity-info [class name]
  (or (.GetField class name)
      (.GetProperty class name)
      (.GetMethod class name Type/EmptyTypes)))

(defn resolve
  ([t]
   (or (clojure.core/resolve t)
       (throw! "Could not resolve " t " as  type.")))
  ([t ast]
   (or (clojure.core/resolve t)
       (throw! "Could not resolve " t " as  type in " (:form ast)))))

(defn clr-type [{:keys [op] :as ast}]
  (condp = op
    :const
    (type (:val ast))
    
    :host-interop
    (let [{:keys [m-or-f target]} ast
          target-type (clr-type target)]
      (or (zero-arity-type target-type (str m-or-f))
          (throw! "Host interop type " (:form ast) " not supported")))
    
    :maybe-host-form
    (let [{:keys [class field]} ast
          class (resolve class)]
      (or (zero-arity-type class (str field))
          (throw! "Maybe host form type " (:form ast) " not supported")))
    
    :maybe-class
    (let [{:keys [class]} ast]
      (resolve class ast))
    
    :invoke
    (let [{:keys [fn args] {:keys [op]} :fn} ast]
      (condp = op
        :maybe-host-form
        (let [{:keys [class field]} fn
              method (or (.GetMethod (resolve class)
                                     (str field)
                                     (into-array (map clr-type args)))
                         (throw! "Could not find method " class "/" field " matching types"))]
          (.ReturnType method))
        
        :var
        (->> fn
             :meta
             :arglists
             (filter #(= (count %) (count args)))
             first
             meta
             :tag
             resolve)
        
        (throw! "Invoking " op " not supported")))
    
    :var
    Object
    
    :new
    (-> ast :class :class resolve)
    
    (throw! "clr-type not implemented for " ast)))

(def load-constant)

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
  (let [from-type (clr-type from)]
    (cond
      (or (nil? from) (nil? from-type))
      nil
      
      ;; do nothing if the types are the same 
      (= from-type to)
      nil
      
      ;; cannot convert nil to value type
      (and (nil? from) (.IsValueType to))
      (throw (Exception. (str "Cannot convert nil to value type " to)))
      
      ;; do nothing for nil to non value type 
      (nil? from)
      nil
      
      ;; convert void to nil
      ;; TODO is this a terrible idea?
      (and (= System.Void from-type) (not (.IsValueType to)))
      (il/ldnull)
      
      (and (= System.Void from-type) (.IsValueType to))
      (throw (Exception. (str "Cannot convert void to value type " to)))
      
      ;; use user defined implicit conversion if it exists
      (find-method from-type "op_Implicit" to)
      (il/call (find-method from-type "op_Implicit" to))
      
      ;; use user defined explicit conversion if it exists
      (find-method from-type "op_Explicit" to)
      (il/call (find-method from-type "op_Explicit" to))
      
      ;; use intrinsic conv opcodes from primitive to primitive
      (and (.IsPrimitive from-type) (.IsPrimitive to))
      (intrinsic-conv to)
      
      ;; box valuetypes to objects
      (and (.IsValueType from-type) (= to Object))
      (il/box from-type)
      
      ;; unbox objects to valuetypes
      (and (= from-type Object) (.IsValueType to))
      (il/unbox-any to)
      
      ;; castclass if to is a subclass of from
      (.IsSubclassOf to from-type)
      (il/castclass to)
      
      ;; do nothing if converting to super class
      (.IsSubclassOf from-type to)
      nil
      
      :else
      (throw (Exception. (str "Cannot convert " from-type " to " to))))))


(defn load-set
  ([v] (load-set v load-constant))
  ([v f]
   [(load-constant (int (count v)))
    (il/newarr Object)
    (map (fn [i c]
           [(il/dup)
            (load-constant (int i))
            (f c)
            (convert c Object)
            (il/stelem-ref)])
         (range)
         v)
    (il/call (find-method clojure.lang.RT "set" |System.Object[]|))]))

(defn load-list
  ([v] (load-list v load-constant))
  ([v f]
   [(load-constant (int (count v)))
    (il/newarr Object)
    (map (fn [i c]
           [(il/dup)
            (load-constant (int i))
            (f c)
            (convert c Object)
            (il/stelem-ref)])
         (range)
         v)
    (il/call (find-method clojure.lang.PersistentList "create" |System.Object[]|))]))

(defn load-keyword [k]
  (let [ns  (.. k Namespace)
        name (.. k Name)]
    [(load-constant ns)
     (load-constant name)
     (il/call (find-method Keyword "intern" String String))]))

(defn load-symbol [k]
  (let [ns  (.. k Namespace)
        name (.. k Name)]
    [(load-constant ns)
     (load-constant name)
     (il/call (find-method Symbol "intern" String String))]))

(defn load-var [v]
  (let [nsname  (.. v Namespace Name ToString)
        symname (.. v Symbol ToString)]
    [(load-constant nsname)
     (load-constant symname)
     (il/call (find-method RT "var" String String))]))

;; NOTE the stock compiler looks up types using RT.classForName
;; if the type is not a valuetype. why? does it make a difference?
(defn load-type [v]
  [(il/ldtoken v)
   (il/call (find-method Type "GetTypeFromHandle" RuntimeTypeHandle))])

(defn get-var [v]
  (if (.isDynamic v)
    (il/call (find-method Var "get"))
    (il/call (find-method Var "getRawRoot"))))

(defn load-regexp [r]
  [(il/ldstr (str r))
   (il/newobj (find-constructor System.Text.RegularExpressions.Regex String))])

(defn load-ratio [r]
  [(il/ldstr (pr-str r))
   (il/call (find-method clojure.lang.RT "readString" String))])

(defn load-bigint [r]
  [(il/ldstr (pr-str r))
   (il/call (find-method clojure.lang.RT "readString" String))])

;; multimethod?
(defn load-constant [k]
  (cond 
    (nil? k)                         (il/ldnull)
    (instance? System.String k)      (il/ldstr k)
    (instance? System.Boolean k)     (if k (il/ldc-i4-1) (il/ldc-i4-0))
    (instance? System.Int32 k)       (il/ldc-i4 k)
    (instance? System.Int64 k)       (il/ldc-i8 k)
    (instance? System.Single k)      (il/ldc-r4 k)
    (instance? System.Double k)      (il/ldc-r8 k)
    (instance? System.Char k)        [(il/ldc-i4-s (int k)) (il/conv-u2)]
    
    (instance? System.Type k)                           (load-type k)
    (instance? System.Text.RegularExpressions.Regex k)  (load-regexp k)
    (instance? clojure.lang.BigInt k)                   (load-bigint k)
    (instance? clojure.lang.Ratio k)                    (load-ratio k)
    (instance? clojure.lang.Symbol k)                   (load-symbol k)
    (instance? clojure.lang.Keyword k)                  (load-keyword k)
    (instance? clojure.lang.Var k)                      (load-var k)
    (instance? clojure.lang.PersistentList k)           (load-list k)
    ;; (instance? clojure.lang.APersistentSet k)           (load-set k)
    ;; (instance? clojure.lang.APersistentVector k)        (load-vector k)
    ;; (instance? clojure.lang.APersistentMap k)           (load-map (seq k))
    ))

(defn to-address [t]
  (let [l (il/local t)]
    [(il/stloc l)
     (il/ldloca l)]))

(defn cleanup-stack
  ([{{:keys [context]} :env}]
   (if (= context :ctx/statement)
     (il/pop)))
  ([lasttype {{:keys [context]} :end}]
   (if-not (= lasttype System.Void)
     (cleanup-stack context))))

(def intrinsics
  {(find-method clojure.lang.RT "uncheckedIntCast" Double)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "uncheckedIntCast" Int64)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "uncheckedIntCast" Single)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "longCast" Int64)
   []
   
   (find-method clojure.lang.RT "longCast" Int32)
   []
   
   (find-method clojure.lang.RT "uncheckedIntCast" Int32)
   []
   
   (find-method clojure.lang.RT "intCast" Int32)
   []
   
   (find-method clojure.lang.RT "intCast" Int64)
   [(il/conv-ovf-i4)]
   
   (find-method clojure.lang.Numbers "unchecked_add" Double Int64)
   [(il/conv-r8)
    (il/add)]
   
   ;; TODO replace Numbers.add with ovf intrinsics when possible? ~40% faster
   (find-method clojure.lang.Numbers "lt" Int64 Int64)
   (il/clt)
   
   (find-method clojure.lang.Numbers "lt" Double Double)
   (il/clt)
   
   (find-method clojure.lang.Numbers "lt" Double Int64)
   (il/clt)
   
   (find-method clojure.lang.Numbers "gt" Int64 Int64)
   (il/cgt)
   
   (find-method clojure.lang.Util "equiv" Int64 Int64)
   (il/ceq)
   
   (find-method clojure.lang.Util "equiv" Double Double)
   (il/ceq)
   
   (find-method clojure.lang.Util "equiv" Object Object)
   (il/ceq)
    
   (find-method clojure.lang.Numbers "inc" Int64)
   [(il/ldc-i8 1)
    (il/add-ovf)]
    
   (find-method clojure.lang.Numbers "unchecked_inc" Int64)
   [(il/ldc-i8 1)
    (il/add)]
   
   (find-method clojure.lang.Numbers "add" Int64 Int64)
   (il/add-ovf)
    
   (find-method clojure.lang.Numbers "add" Double Int64)
   [(il/conv-r8)
    (il/add-ovf)]
      
   (find-method clojure.lang.Numbers "add" Double Double)
   (il/add)
   
   (find-method clojure.lang.Numbers "unchecked_add" Double Double)
   (il/add)
   
   (find-method clojure.lang.Numbers "unchecked_add" Int64 Int64)
   (il/add)
   
   (find-method clojure.lang.Numbers "unchecked_multiply" Int64 Int64)
   (il/mul)
   
   (find-method clojure.lang.Numbers "unchecked_multiply" Double Double)
   (il/mul)
   
   (find-method clojure.lang.Numbers "unchecked_multiply" Double Int64)
   [(il/conv-r8)
    (il/mul)]
   
   (find-method clojure.lang.Numbers "unchecked_multiply" Int64 Double)
   [(let [loc (il/local Double)]
      [(il/stloc loc)
       (il/conv-r8)
       (il/ldloc loc)
       (il/mul)])]
   })

(defn has-arity-method [arities]
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

;; ast -symbolize-> symbolics -emit-> bytecode
;;        M&GIC                M&GE

(def symbolize)

;; 42
;; "foo"
(defn literal-symbolizer
  [{:keys [val] :as ast}
   symbolizers]
  [(load-constant val)
   (cleanup-stack ast)])

;; [1 2 3]
(defn vector-symbolizer
  [{:keys [items]} symbolizers]
  [(load-constant (int (count items)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (symbolize c symbolizers)
           (convert c Object)
           (il/stelem-ref)])
        (range)
        items)
   (il/call (find-method clojure.lang.RT "vector" |System.Object[]|))])

;; #{1 2 3}
(defn set-symbolizer
  [{:keys [items]} symbolizers]
  [(load-constant (int (count items)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (symbolize c symbolizers)
           (convert c Object)
           (il/stelem-ref)])
        (range)
        items)
   (il/call (find-method clojure.lang.RT "set" |System.Object[]|))])

;; {:foo bar}
(defn map-symbolizer
  [{:keys [keys vals]} symbolizers]
  [(load-constant (int (+ (count keys) (count vals))))
   (il/newarr Object)
   (map (fn [i kv]
          [(il/dup)
           (load-constant (int i))
           (symbolize kv symbolizers)
           (convert kv Object)
           (il/stelem-ref)])
        (range)
        (interleave keys vals))
   (il/call (find-method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))])

;; (f a b)
(defn invoke-symbolizer
  [{:keys [fn args] :as ast} symbolizers]
  (condp = (:op fn)
    :maybe-host-form
    (let [{:keys [class field]} fn
          method (or (.GetMethod (resolve class)
                                 (str field)
                                 (into-array (map clr-type args)))
                     (throw! "Could not find method "
                             class "." field
                             "(" (string/join ","
                                              (map clr-type args)) ")"))
          method-argument-types (->> method
                                     .GetParameters
                                     (map #(.ParameterType %)))]
      [(map #(vector (symbolize %1 symbolizers)
                     (convert %1 %2))
            args
            method-argument-types)
       (il/call method)
       (cleanup-stack ast)])
    
    [(symbolize fn symbolizers)
     (il/castclass IFn)
     (map #(vector (symbolize % symbolizers)
                   (convert % Object))
          args)
     (il/callvirt (apply find-method IFn "invoke" (repeat (count args) Object)))
     (cleanup-stack ast)]))

;; (new Foo)
(defn new-symbolizer
  [{:keys [args class] :as ast} symbolizers]
  (let [type (clr-type class)
        arg-types (map clr-type args)
        ctor (.GetConstructor type (into-array arg-types))]
    (cond
      ;; have constructor, normal newobj path
      ctor 
      (let [ctor-param-types (->> ctor .GetParameters (map #(.ParameterType %)))]
        ;; TODO what about LocalBindings?
        [(interleave
           (map #(symbolize % symbolizers)
                args)
           (map #(convert %1 %2)
                args
                ctor-param-types))
         (il/newobj ctor)])
      
      ;; no constructor, might be initobj path
      (.IsValueType type) 
      (let [loc (il/local type)]
        [(il/ldloca-s loc)
         (il/initobj type)
         (il/ldloc loc)])
      
      :else
      (throw! "No constructor for non-valuetype " type))))

(defn var-symbolizer
  [{:keys [var] :as ast} symbolizers]
  [(load-var var)
   (get-var var)
   (cleanup-stack ast)])

(defn converted-args [args param-types symbolizers]
  (interleave
    (->> args
         (map :ArgExpr)
         (map #(symbolize % symbolizers)))
    (map #(convert %1 %2)
         (map :ArgExpr args)
         param-types)))

;; interop

;; (+ 1 2)
;; (Foo/Bar a b)
(defn static-method-symbolizer
  [ast symbolizers]
  (let [data (data-map ast)
        typ (:_type data)
        mname (:_methodName data)
        pcon (.ParsedContext ast)
        args (map data-map (:_args data))
        arg-exprs (map :ArgExpr args)
        method (or (:_method data)
                   (apply find-method typ mname (map clr-type arg-exprs)))
        _ (if-not method
            (throw (Exception. (str typ mname (apply str (map clr-type arg-exprs)) ))))
        method-parameter-types (->> method
                                    .GetParameters
                                    (map #(.ParameterType %)))]
    [(converted-args args method-parameter-types symbolizers)
     (if-let [intrinsic-bytecode (intrinsics method)]
       intrinsic-bytecode
       (il/call method))
     (cleanup-stack (.ReturnType method) pcon)]))

(defn call-instance-method
  [target method args symbolizers]
  (let [target-type (.ClrType target)
        method-parameter-types (->> method
                                    .GetParameters
                                    (map #(.ParameterType %))) ]
    [(symbolize target symbolizers)
     (if (.IsValueType target-type)
       (to-address target-type))
     (converted-args args method-parameter-types symbolizers) 
     (if (.IsValueType target-type)
       (il/call method)
       (il/callvirt method))]))

(defn instance-method-symbolizer
  [ast symbolizers]
  (let [data (data-map ast)
        pcon (.ParsedContext ast)
        target (:_target data)
        args (map data-map (:_args data))
        method (:_method data)]
    
    [(call-instance-method target method args symbolizers)
     (cleanup-stack (.ReturnType method)
                    pcon)]))

(defn static-property-symbolizer
  [ast symbolizers]
  (let [pcon (.ParsedContext ast)
        return-type (.ClrType ast)
        getter (-> ast data-map :_tinfo .GetGetMethod)]
    [(il/call getter)
     (cleanup-stack pcon)]))

; (defn static-field-symbolizer
;   [ast symbolizers]
;   (let [{:keys [_tinfo] :as data} (data-map ast)
;         pcon (.ParsedContext ast)
;         return-type (clr-type ast)]
;     [(if (.IsLiteral _tinfo)
;        (load-constant (.GetRawConstantValue _tinfo))
;        (il/ldsfld _tinfo) )
;      (cleanup-stack pcon)]))

(defn get-instance-property
  [target property symbolizers]
  (let [getter (.GetGetMethod property)]
    [(symbolize target symbolizers)
     (convert target Object)
     (il/callvirt getter)]))

(defn instance-property-symbolizer
  [ast symbolizers]
  (let [data (data-map ast)
        pcon (.ParsedContext ast)
        target (:_target data)
        property (-> data :_tinfo)]
    [(get-instance-property target property symbolizers)
     (cleanup-stack pcon)]))

(defn get-instance-field
  [target field symbolizers]
  [(symbolize target symbolizers)
   (il/ldfld field)])

(defn instance-field-symbolizer
  [ast symbolizers]
  (let [data (data-map ast)
        pcon (.ParsedContext ast)
        target (:_target data)
        field (:_tinfo data)]
    [(get-instance-field target field symbolizers)
     (cleanup-stack pcon)]))

(defn fn-symbolizer
  [{:keys [methods] :as ast} symbolizers]
  (let [name (str (gensym "fn"))
        arities (map :fixed-arity methods)]
    (mage.core/type
      name
      TypeAttributes/Public []
      clojure.lang.AFn
      [(il/constructor
         MethodAttributes/Public
         CallingConventions/Standard []
         (il/ret))
       (il/constructor
         (enum-or MethodAttributes/Static)
         CallingConventions/Standard []
         [#_ (map (fn [[v fld]] [(load-var v) (il/stsfld fld)])
                  var-fields)
          #_ (map (fn [[k fld]] [(load-constant k) (il/stsfld fld)])
                  constant-fields)
          (il/ret)])
       (has-arity-method arities)
       (map #(symbolize % symbolizers) methods)])))

(defn fn-method-symbolizer
  [{:keys [body params] {:keys [ret statements]} :body} symbolizers]
  
  (il/method "invoke"
             (enum-or MethodAttributes/Public
                      MethodAttributes/Virtual)
             Object (mapv (constantly Object) params)
             [(symbolize body symbolizers)
              (symbolize ret symbolizers)
              (convert ret Object)
              (il/ret)]))

(defn if-symbolizer
  [ast symbolizers]
  (let [{:keys [_testExpr _thenExpr _elseExpr] :as data} (data-map ast)
        pcon (.ParsedContext ast)
        false-label (il/label)
        end-label (il/label)]
    [(symbolize _testExpr symbolizers)
     (il/brfalse false-label)
     (symbolize _thenExpr symbolizers)
     (il/br end-label)
     false-label
     (symbolize _elseExpr symbolizers)
     end-label]))

(defn body-symbolizer
  [ast symbolizers]
  [(map #(symbolize % symbolizers) (-> ast data-map :_exprs))])

;; TODO is this nonsensical? throw an error?
(defn local-binding-symbolizer
  [ast symbolizers]
  (let [{:keys [IsArg Index ClrType]} (-> ast data-map :Binding data-map)]
    (if IsArg
      (load-argument Index)
      (il/ldloc (il/local (or ClrType Object))))))

(defn let-symbolizer
  [ast symbolizers]
  (let [{:keys [_bindingInits _body]} (-> ast data-map)
        pcon (.ParsedContext ast)
        bindings (map #(.Binding %) _bindingInits)
        binding-map (->> (interleave bindings
                                     (map #(il/local (clr-type (.Init %))) bindings))
                         (apply hash-map))
        recur-target (il/label)
        specialized-symbolizers
        (assoc symbolizers
          RecurExpr
          (fn let-recur-symbolizer
            [ast symbolizers]
            (let [{:keys [_args _loopLocals]} (data-map ast)]
              [(interleave
                 (map #(symbolize % symbolizers) _args)
                 (map #(if (.IsArg %)
                         (il/starg (.Index %)) ;; TODO is this starg right?
                         (il/stloc (binding-map %)))
                      _loopLocals))
               (il/br recur-target)]))
          LocalBindingExpr
          (fn let-body-symbolizer [ast syms]
            (if-let [loc (-> ast data-map :Binding binding-map)]
              (il/ldloc loc)
              (symbolize ast symbolizers))))]
    
    ;; emit local initializations
    [(map (fn [b loc]
            [(symbolize (.Init b) specialized-symbolizers)
             (il/stloc loc)])
          bindings
          (map binding-map bindings))
    
    ;; mark recur target
     recur-target
     
     ;; emit body with specialized symbolizers
     (symbolize _body specialized-symbolizers)
     ; (cleanup-stack pcon)
     ]))

(defn monitor-enter-symbolizer
  [ast symbolizers]
  (let [{:keys [_target]} (data-map ast)]
    [(symbolize _target symbolizers)
     (il/call (find-method Monitor "Enter" Object))
     (il/ldnull)]))

(defn monitor-exit-symbolizer
  [ast symbolizers]
  (let [{:keys [_target]} (data-map ast)]
    [(symbolize _target symbolizers)
     (il/call (find-method Monitor "Exit" Object))
     (il/ldnull)]))

(defn instance-of-symbolizer
  [ast symbolizers]
  (let [{:keys [_expr _t]} (data-map ast)]
    [(symbolize _expr symbolizers)
     (convert _expr Object)
     (il/isinst _t)
     (il/ldnull)
     (il/cgt-un)]))

(defn throw-symbolizer
  [ast symbolizers]
  (let [{:keys [_excExpr]} (data-map ast)]
    [(symbolize _excExpr symbolizers)
     (convert _excExpr Exception)
     (il/throw)]))

(defn try-symbolizer
  [ast symbolizers]
  (let [{:keys [_tryExpr _finallyExpr _catchExprs]} (data-map ast)
        expr-type (clr-type ast)
        ret (il/local expr-type)
        catch-symbolizer
        (fn catch-symbolizer
          [ast symbolizers]
          (let [{:keys [_type _lb _handler]} (data-map ast)
                exception-local (il/local _type)
                catch-binding-symbolizers
                (assoc symbolizers LocalBindingExpr
                  (fn catch-body-symbolizer [ast syms]
                    (if (= _lb (-> ast data-map :Binding))
                      (il/ldloc exception-local)
                      (symbolize ast syms))))]
            (il/catch _type
                      [(il/stloc exception-local)
                       (symbolize _handler catch-binding-symbolizers)
                       (convert _handler expr-type)
                       (il/stloc ret)])))
        catch-symbolizers (assoc symbolizers
                            TryExpr+CatchClause
                            catch-symbolizer)]
    (if (and (empty? _catchExprs)
             (nil? _finallyExpr))
      (symbolize _tryExpr symbolizers)
      [(il/exception
         [(symbolize _tryExpr symbolizers)
          (convert _tryExpr expr-type)
          (il/stloc ret)
          (map #(catch-symbolizer % catch-symbolizers) _catchExprs)
          (if _finallyExpr
            (il/finally (symbolize _finallyExpr symbolizers)))])
       (il/ldloc ret)])))

(defn unresolved-var-symbolizer
  [ast symbolizers]
  nil)

(defn empty-symbolizer
  [ast symbolizers]
  (let [{:keys [_coll]} (data-map ast)]
    (cond
      (instance? IPersistentList _coll)   (il/ldsfld (.GetField PersistentList "EMPTY"))
      (instance? LazySeq _coll)           (il/ldsfld (.GetField PersistentList "EMPTY"))
      (instance? IPersistentVector _coll) (il/ldsfld (.GetField PersistentVector "EMPTY"))
      (instance? IPersistentMap _coll)    (il/ldsfld (.GetField PersistentArrayMap "EMPTY"))
      (instance? IPersistentSet _coll)    (il/ldsfld (.GetField PersistentHashSet "EMPTY"))
      :else                               (throw (InvalidOperationException. "Unknown collection type.")))))

(defn import-symbolizer
  [ast symbolizers]
  (let [cls (-> ast data-map :_c)]
    [(il/call (property-getter clojure.lang.Compiler "CurrentNamespace"))
     (load-constant cls)
     (il/call (find-method clojure.lang.RT "classForName" String))
     (il/call (find-method clojure.lang.Namespace "importClass" Type))]))

(defn keyword-invoke-symbolizer
  [ast symbolizers]
  (let [{:keys [_kw _target]} (data-map ast)
        pcon (.ParsedContext ast)]
    [(symbolize _kw symbolizers)
    (il/castclass IFn)
    (symbolize _target symbolizers)
    (convert _target Object)
    (il/callvirt (find-method IFn "invoke" Object))
    (cleanup-stack pcon)]))

(defn case-shift-mask [shift mask]
  (if-not (zero? mask)
    [(load-constant shift)
     (il/shr)
     (load-constant mask)
     (il/and)]))

(defn case-int-expr [ast default-label symbolizers]
  (let [{:keys [_expr _shift _mask]} (data-map ast)]
    [(symbolize _expr symbolizers)
     (il/call (find-method clojure.lang.Util "IsNonCharNumeric" Object))
     (il/brfalse default-label)
     (symbolize _expr symbolizers)
     (convert _expr Int32)
     (case-shift-mask _shift _mask)]))

(defn case-hash-expr [ast default-label symbolizers]
  (let [{:keys [_expr _shift _mask]} (data-map ast)]
    [(symbolize _expr symbolizers)
     (il/call (find-method clojure.lang.Util "hash" Object))
     (case-shift-mask _shift _mask)]))

(defn case-symbolizer
  [ast symbolizers]
  (let [{:keys [_expr _shift _mask _low _high
                _defaultExpr _tests _thens
                _switchType _testType _skipCheck]} (data-map ast)
        default-label (il/label)
        labels (repeatedly (count (.Keys _tests)) il/label) ;; TODO too simplistic?
        ]
    [(if (= _testType :int)
      (case-int-expr ast default-label symbolizers)
      (case-hash-expr ast default-label symbolizers))
     (if (= _switchType :sparse)
       [(il/switch labels)
        (il/br default-label)]
       (map (fn [i]
              )
            (range _low _high))
       
       )
     ]
    ))

(defn reflective-instance-zero-arity-call-symbolizer
  [ast symbolizers]
  (let [{:keys [_target _memberName]} (data-map ast)
        target (il/local)
        target-type (il/local Type)
        property-branch (il/label)
        arity-zero-branch (il/label)
        failed-branch (il/label)
        end (il/label)]
    [(symbolize _target symbolizers)
     (il/stloc target)
     (il/ldloc target)
     (il/callvirt (find-method Object "GetType"))
     (il/stloc target-type)
     
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetField" Type String Boolean))
     (il/dup)
     (il/brfalse property-branch)
     (il/castclass FieldInfo)
     (il/ldloc target)
     (il/callvirt (find-method FieldInfo "GetValue" Object))
     (il/br end)
     
     property-branch
     (il/pop)
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetProperty" Type String Boolean))
     (il/dup)
     (il/brfalse arity-zero-branch)
     (il/castclass PropertyInfo)
     (il/ldloc target)
     (il/ldnull)
     (il/callvirt (find-method PropertyInfo "GetValue" Object |System.Object[]|))
     (il/br end)
     
     arity-zero-branch
     (il/pop)
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetArityZeroMethod" Type String Boolean))
     (il/dup)
     (il/brfalse failed-branch)
     (il/castclass MethodInfo)
     (il/ldloc target)
     (il/ldnull)
     (il/callvirt (find-method MethodInfo "Invoke" Object |System.Object[]|))
     (il/br end)
     
     failed-branch
     (il/ldstr (str "Reflective call to " _memberName " falied!"))
     (il/newobj (find-constructor Exception String))
     (il/throw)
     end]))

(defn instance-zero-arity-call-symbolizer
  [ast symbolizers]
  (let [{:keys [_target _memberName]} (data-map ast)
        typ (clr-type _target)]
    (if-let [info (.GetField typ _memberName)]
      (get-instance-field _target info symbolizers)
      (if-let [info (.GetProperty typ _memberName)]
        (get-instance-property _target info symbolizers)
        (if-let [info (find-method typ _memberName)]
          (call-instance-method _target info [] symbolizers)
          (reflective-instance-zero-arity-call-symbolizer ast symbolizers))))))

(defn do-symbolizer
  [{:keys [statements]} symbolizers]
  (map #(symbolize % symbolizers) statements))

(defn load-static-field [field-info]
  (if (.IsLiteral field-info)
    (load-constant (.GetRawConstantValue field-info))
    (il/ldsfld field-info)))

;; Foo/Bar
(defn host-form-symbolizer
  [{:keys [class field] :as ast} symbolizers]
  [(or (if-let [info (.GetField (resolve class) (str field))]
         (load-static-field info))
       (if-let [info (.GetProperty (resolve class) (str field))]
         (il/call (.GetGetMethod info)))
       (if-let [info (.GetMethod (resolve class) (str field) Type/EmptyTypes)]
         (il/call info))
       (throw (Exception. (str field " in " (:form ast) " not a field, property, or method."))))
   (cleanup-stack ast)])

(defn host-interop-symbolizer
  [{:keys [m-or-f target] :as ast} symbolizers]
  (let [target-type (clr-type target)
        morf (str m-or-f)]
    [(symbolize target symbolizers)
     (or (if-let [info (.GetField target-type morf)]
           (il/ldfld info))
         (if-let [info (.GetProperty target-type morf)]
           (il/callvirt (.GetGetMethod info)))
         (if-let [info (.GetMethod target-type morf Type/EmptyTypes)]
           (il/callvirt info)))
     (cleanup-stack ast)]))

(def base-symbolizers
  {:const           literal-symbolizer
   :vector          vector-symbolizer
   :set             set-symbolizer
   :map             map-symbolizer
   :invoke          invoke-symbolizer
   :var             var-symbolizer
   :do              do-symbolizer
   :fn              fn-symbolizer
   :fn-method       fn-method-symbolizer
   :maybe-host-form host-form-symbolizer
   :host-interop    host-interop-symbolizer
   :new             new-symbolizer})

(defn ast->symbolizer [ast symbolizers]
  (or (-> ast :op symbolizers)
      (throw (Exception. (str "No symbolizer for " (pr-str (or  (:op ast)
                                                               ast)))))))

(defn symbolize [ast symbolizers]
  (if-let [symbolizer (ast->symbolizer ast symbolizers)]
    (symbolizer ast symbolizers)))

(comment
(defn cached-instance-zero-arity-call-symbolizer
  [ast symbolizers]
  (let [{:keys [_target _memberName]} (data-map ast)
        private-static (enum-or FieldAttributes/Private FieldAttributes/Static)
        cached-target-type (il/field Type
                                     private-static
                                     (gensym (str "cached_" _memberName "_target_type")))
        cached-info (il/field Object
                              private-static
                              (gensym (str "cached_" _memberName "_info")))
        cached-info-type (il/field Type
                                   private-static
                                   (gensym (str "cached_" _memberName "_info_type")))
        target (il/local)
        target-type (il/local Type)
        property-branch (il/label)
        arity-zero-branch (il/label)
        failed-branch (il/label)
        missed-branch (il/label)
        end (il/label)
        ]
    [;; symbolize and store target and its type
     (symbolize _target symbolizers)
     (il/stloc target)
     (il/ldloc target)
     (il/callvirt (find-method Object "GetType"))
     (il/stloc target-type)
     
     ;; is the incoming target's type the same as the cached type?
     (il/ldloc target-type)
     (il/ldsfld cached-target-type)
     (il/ceq)
     (il/brfalse missed-branch)
     
     ;; if so, cast the cached info and invoke it
     (let [try-prop (il/label)
           try-meth (il/label)]
       [(il/ldsfld cached-info-type)
        (load-constant FieldInfo)
        (il/ceq)
        (il/brfalse try-prop)
        (il/ldsfld cached-info)
        (il/castclass FieldInfo)
        (il/ldloc target)
        (il/callvirt (find-method FieldInfo "GetValue" Object))
        (il/br end)
        
        try-prop
        (il/ldsfld cached-info-type)
        (load-constant PropertyInfo)
        (il/ceq)
        (il/brfalse try-meth)
        (il/ldsfld cached-info)
        (il/castclass PropertyInfo)
        (il/ldloc target)
        (il/ldnull)
        (il/callvirt (find-method PropertyInfo "GetValue" Object |System.Object[]|))
        (il/br end)
        
        try-meth
        (il/ldsfld cached-info-type)
        (load-constant MethodInfo)
        (il/ceq)
        (il/brfalse missed-branch)
        (il/ldsfld cached-info)
        (il/castclass MethodInfo)
        (il/ldloc target)
        (il/ldnull)
        (il/callvirt (find-method MethodInfo "Invoke" Object |System.Object[]|))
        (il/br end)])

     missed-branch
     ;; cache target type for next time
     (il/ldloc target-type)
     (il/stsfld cached-target-type)
     
     ;; try and get field
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetField" Type String Boolean))
     (il/dup)
     (il/brfalse property-branch)
     ;; FieldInfo found! save in cache and call GetValue
     (load-constant FieldInfo)
     (il/stsfld cached-info-type)
     (il/castclass FieldInfo)
     (il/dup)
     (il/stsfld cached-info)
     (il/ldloc target)
     (il/callvirt (find-method FieldInfo "GetValue" Object))
     (il/br end)
     
     ;; try and get property
     property-branch
     (il/pop)
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetProperty" Type String Boolean))
     (il/dup)
     (il/brfalse arity-zero-branch)
     ;; PropertyInfo found! save in cache and call GetValue  
     (load-constant PropertyInfo)
     (il/stsfld cached-info-type)
     (il/castclass PropertyInfo)
     (il/dup)
     (il/stsfld cached-info)
     (il/ldloc target)
     (il/ldnull)
     (il/callvirt (find-method PropertyInfo "GetValue" Object |System.Object[]|))
     (il/br end)
     
     ;; try and get arity zero method
     arity-zero-branch
     (il/pop)
     (il/ldloc target-type)
     (load-constant _memberName)
     (il/ldc-i4-0)
     (il/call (find-method clojure.lang.Reflector "GetArityZeroMethod" Type String Boolean))
     (il/dup)
     (il/brfalse failed-branch)
     ;; MethodInfo found! save in cache and call GetValue  
     (load-constant MethodInfo)
     (il/stsfld cached-info-type)
     (il/castclass MethodInfo)
     (il/dup)
     (il/stsfld cached-info)
     (il/ldloc target)
     (il/ldnull)
     (il/callvirt (find-method MethodInfo "Invoke" Object |System.Object[]|))
     (il/br end)
     
     failed-branch
     (il/ldstr (str "Reflective call to " _memberName " falied!"))
     (il/newobj (find-constructor Exception String))
     (il/throw)
     end])))

(defn compile-fn [expr]
  (let [asm-name "magic.tests"]
    (-> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (symbolize (ana/ast expr) base-symbolizers)))
        il/emit!
        :mage.core/assembly-builder
        .GetTypes
        first
        Activator/CreateInstance
        )))

(defmacro magic-defn [name args & body]
  `(def ~name (magic-compile-fn '(fn ~name ~args ~@body))))
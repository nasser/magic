;; this is magic
;; should be clojure.tools.analyzer.clj
(ns magic.old
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
           ; [System.Threading Monitor]
           [System.Reflection TypeAttributes MethodAttributes FieldAttributes FieldInfo MethodInfo PropertyInfo]
           System.AppDomain
           System.Reflection.Emit.OpCodes
           ; AssemblyName
           ; AssemblyBuilderAccess
           ))

(require '[magic.analyzer :as ana])

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

(defn find-field
  ([type name] (.GetField type name)))

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
    (= i 0) (il/ldarg-1)
    (= i 1) (il/ldarg-2)
    (= i 2) (il/ldarg-3)
    :else (il/ldarg i)))

(defmulti ast-type :op)

(defmethod ast-type :default [ast]
  (throw! "ast-type not implemented for " (pr-str ast)))

;; TODO repeated shape between instance-zero-arity-call-type instance-zero-arity-call-compiler
(defn instance-zero-arity-call-type [izac]
  (let [{:keys [_target _memberName]} (data-map izac)
        typ (ast-type _target)]
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

(defn n-arity-type [class method params]
  (if-let [info (.GetMethod class name params)]
    (.ReturnType info)))

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


(defmethod ast-type :maybe-class
  [{:keys [class] :as ast}]
  (resolve class ast))

(defmethod ast-type :var [ast]
  Object)

(defmethod ast-type :the-var [ast]
  Object)

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
  (let [from-type (ast-type from)]
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
      
      ;; RT casts
      (and (= from-type Object) (= to Single))
      (il/call (find-method RT "floatCast" from-type))
      (and (= from-type Object) (= to Double))
      (il/call (find-method RT "doubleCast" from-type))
      (and (= from-type Object) (= to Int32))
      (il/call (find-method RT "intCast" from-type))
      (and (= from-type Object) (= to Int64))
      (il/call (find-method RT "longCast" from-type))
      
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

;; ast -compile-> symbolics -emit-> bytecode
;;        M&GIC                M&GE

(def compile)

;; 42
;; "foo"
(defmethod ast-type :const [ast]
  (-> ast :val type))

(defn literal-compiler
  [{:keys [val] :as ast}
   compilers]
  [(load-constant val)
   (cleanup-stack ast)])

;; [1 2 3]
(defmethod ast-type :vector [ast]
  clojure.lang.IPersistentVector)

(defn vector-compiler
  [{:keys [items]} compilers]
  [(load-constant (int (count items)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (compile c compilers)
           (convert c Object)
           (il/stelem-ref)])
        (range)
        items)
   (il/call (find-method clojure.lang.RT "vector" |System.Object[]|))])

;; #{1 2 3}
(defmethod ast-type :set [ast]
  clojure.lang.IPersistentSet)

(defn set-compiler
  [{:keys [items]} compilers]
  [(load-constant (int (count items)))
   (il/newarr Object)
   (map (fn [i c]
          [(il/dup)
           (load-constant (int i))
           (compile c compilers)
           (convert c Object)
           (il/stelem-ref)])
        (range)
        items)
   (il/call (find-method clojure.lang.RT "set" |System.Object[]|))])

;; {:foo bar}
(defmethod ast-type :map [ast]
  clojure.lang.IPersistentMap)

(defn map-compiler
  [{:keys [keys vals]} compilers]
  [(load-constant (int (+ (count keys) (count vals))))
   (il/newarr Object)
   (map (fn [i kv]
          [(il/dup)
           (load-constant (int i))
           (compile kv compilers)
           (convert kv Object)
           (il/stelem-ref)])
        (range)
        (interleave keys vals))
   (il/call (find-method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))])

;; (fn args...)
(defmethod ast-type :invoke
  [{:keys [fn args] {:keys [op]} :fn}]
  (condp = op
    ;; (class/field args...)
    :maybe-host-form
    (let [{:keys [class field]} fn
          method (or (.GetMethod (resolve class)
                                 (str field)
                                 (into-array (map ast-type args)))
                     (throw! "Could not find method " class "/" field " matching types"))]
      (.ReturnType method))
    
    ;; (fn args...)
    :var
    (resolve (or (->> fn
                      :meta
                      :arglists
                      (filter #(= (count %) (count args)))
                      first
                      meta
                      :tag)
                 'Object))
    
    (ast-type fn)
    ; (throw! "Invoking " op " not supported")
    ))

(def intrinsic-vars
  {[#'clojure.core/+ Int32 Int32]
   (il/add)})

(defn invoke-compiler
  [{:keys [fn args] :as ast} compilers]
  (cond
    (= (:op fn) :maybe-host-form)
    (let [{:keys [class field]} fn
          method (or (.GetMethod (resolve class)
                                 (str field)
                                 (into-array (map ast-type args)))
                     (throw! "Could not find method "
                             class "." field
                             "(" (string/join ","
                                              (map ast-type args)) ")"))
          method-argument-types (->> method
                                     .GetParameters
                                     (map #(.ParameterType %)))]
      [(map #(vector (compile %1 compilers)
                     (convert %1 %2))
            args
            method-argument-types)
       (il/call method)
       (cleanup-stack ast)])
    
    (intrinsic-vars (vec (conj (map #(-> % :form meta :tag) args)
                               (:var fn))))
    [(intrinsic-vars (vec (conj (map #(-> % :form meta :tag) args)
                                (:var fn))))
     (map #(vector (compile % compilers))
          args)]
     
     :else
    [(compile fn compilers)
     (il/castclass IFn)
     (map #(vector (compile % compilers)
                   (convert % Object))
          args)
     (il/callvirt (apply find-method IFn "invoke" (repeat (count args) Object)))
     (cleanup-stack ast)]))

;; (new Foo)
(defmethod ast-type :new [ast]
  (-> ast :class :class resolve))

(defn new-compiler
  [{:keys [args class] :as ast} compilers]
  (let [type (ast-type class)
        arg-types (map ast-type args)
        ctor (.GetConstructor type (into-array arg-types))]
    (cond
      ;; have constructor, normal newobj path
      ctor 
      (let [ctor-param-types (->> ctor .GetParameters (map #(.ParameterType %)))]
        ;; TODO what about LocalBindings?
        [(interleave
           (map #(compile % compilers)
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

(defn var-compiler
  [{:keys [var] :as ast} compilers]
  [(load-var var)
   (get-var var)
   (cleanup-stack ast)])

(defn fn-compiler
  [{:keys [methods] :as ast} compilers]
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
       (map #(compile % compilers) methods)])))

(defn fn-method-compiler
  [{:keys [body params] {:keys [ret statements]} :body} compilers]
  (il/method "invoke"
             (enum-or MethodAttributes/Public
                      MethodAttributes/Virtual)
             ;; Object (mapv (constantly Object) params)
             (ast-type ret) (mapv ast-type params)
             [(compile body compilers)
              (convert ret Object)
              (il/ret)]))

(defn local-compiler
  [{:keys [name arg-id local] :as ast} compilers]
  (if (= local :arg)
    (load-argument arg-id)
    (throw! "Local " name " not an argument and could not be compiled")))

;; (do statements...)
(defn do-compiler
  [{:keys [statements ret]} compilers]
  [(map #(compile % compilers) statements)
   (compile ret compilers)])

(defn load-static-field [field-info]
  (if (.IsLiteral field-info)
    (load-constant (.GetRawConstantValue field-info))
    (il/ldsfld field-info)))

;; class/field
(defmethod ast-type :maybe-host-form
  [{:keys [class field] :as ast}]
  (let [class (resolve class)]
    (or (zero-arity-type class (str field))
        (throw! "Maybe host form type " (:form ast) " not supported"))))

(defn host-form-compiler
  [{:keys [class field] :as ast} compilers]
  [(or (if-let [info (.GetField (resolve class) (str field))]
         (load-static-field info))
       (if-let [info (.GetProperty (resolve class) (str field))]
         (il/call (.GetGetMethod info)))
       (if-let [info (.GetMethod (resolve class) (str field) Type/EmptyTypes)]
         (il/call info))
       (throw (Exception. (str field " in " (:form ast) " not a field, property, or method."))))
   (cleanup-stack ast)])

;; (. target m-or-f)
(defmethod ast-type :host-interop
  [{:keys [m-or-f target] :as ast}]
  (let [target-type (ast-type target)]
    (or (zero-arity-type target-type (str m-or-f))
        (throw! "Host interop " (:form ast) " not supported"))))

(defn host-interop-compiler
  [{:keys [m-or-f target] :as ast} compilers]
  (let [target-type (ast-type target)
        morf (str m-or-f)]
    [(compile target compilers)
     (convert target target-type)
     (or (if-let [info (.GetField target-type morf)]
           (il/ldfld info))
         (if-let [info (.GetProperty target-type morf)]
           (il/callvirt (.GetGetMethod info)))
         (if-let [info (.GetMethod target-type morf Type/EmptyTypes)]
           (il/callvirt info)))
     (cleanup-stack ast)]))

;; (let [a 1] b)
(defmethod ast-type :binding [ast]
  (or
    (if-let [init (:init ast)]
      (ast-type init))
    (if-let [tag (-> ast :name meta :tag)]
      (resolve tag))
    Object))

(defmethod ast-type :local
  [{:keys [name local arg-id] {:keys [locals]} :env}]
  (if (= local :arg)
    (if-let [tag (-> name meta :tag)]
      (resolve tag)
      Object)
    (ast-type (locals name))))

(defmethod ast-type :let [ast]
  (-> ast :body :ret ast-type))

(defn binding-compiler
  [{:keys [bindings body] :as ast} compilers])

(defn let-compiler
  [{:keys [bindings body] :as ast} compilers]
  (let [binding-map (reduce (fn [m binding]
                              (assoc m
                                ;; dissoc env because its not in :locals
                                (dissoc binding :env) 
                                (il/local (ast-type binding))))
                            {} bindings)
        recur-target (il/label)
        specialized-compilers
        (assoc compilers
          :local
          (fn let-local-compiler
            [{:keys [name] {:keys [locals]} :env :as ast} syms]
            (if-let [loc (-> name locals binding-map)]
              (il/ldloc 'loc)
              (compile ast compilers))))]
    
    ;; emit local initializations
    [(map (fn [b]
            [(compile b specialized-compilers)
             (il/stloc (binding-map (dissoc b :env)))])
          bindings)
     
     ;; mark recur target
     recur-target
     
     ;; emit body with specialized compilers
     (compile body specialized-compilers)
     (cleanup-stack ast)]))

(defn binding-compiler
  [{:keys [init] :as ast} compilers]
  [(compile init compilers)
  (cleanup-stack ast)])

(defmethod ast-type :if
  [{:keys [test then else] :as ast}]
  (let [test-type (ast-type test)
        else-type (ast-type else)]
    (if (= test-type else-type)
      test-type
      ;; TODO compute common type  
      Object)))

(defn if-compiler
  [{:keys [test then else] :as ast} compilers]
  (let [false-label (il/label)
        end-label (il/label)]
    [(compile test compilers)
     (convert test Boolean)
     (il/brfalse false-label)
     (compile then compilers)
     (cleanup-stack then)
     (il/br end-label)
     false-label
     (compile else compilers)
     (cleanup-stack else)
     end-label
     ]))

(def base-compilers
  {:const           #'literal-compiler
   :vector          #'vector-compiler
   :set             #'set-compiler
   :map             #'map-compiler
   :invoke          #'invoke-compiler
   :var             #'var-compiler
   :the-var         #'var-compiler
   :do              #'do-compiler
   :fn              #'fn-compiler
   :fn-method       #'fn-method-compiler
   :maybe-host-form #'host-form-compiler
   :host-interop    #'host-interop-compiler
   :new             #'new-compiler
   :let             #'let-compiler
   :binding         #'binding-compiler
   :local           #'local-compiler
   :if              #'if-compiler})

(defn ast->compiler [ast compilers]
  (or (-> ast :op compilers)
      (throw (Exception. (str "No compiler for " (pr-str (or  (:op ast)
                                                               ast)))))))

(defn compile [ast compilers]
  (if-let [compiler (ast->compiler ast compilers)]
    (compiler ast compilers)))

(defn compile-fn [expr]
  (let [asm-name "magic.tests"]
    (-> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (compile (ana/ast expr) base-compilers)))
        il/emit!
        :mage.core/assembly-builder
        .GetTypes
        first
        Activator/CreateInstance
        )))

(defmacro magic-defn [name args & body]
  `(def ~name (magic-compile-fn '(fn ~name ~args ~@body))))
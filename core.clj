;; this is magic
(ns magic.core
  (:refer-clojure :exclude [compile])
  (:require [mage.core :as il]
            [clojure.string :as string])
  (:import [clojure.lang RT Numbers Compiler LineNumberingTextReader
            Symbol Namespace IFn Var Keyword Symbol]
           [clojure.lang.CljCompiler.Ast RHC ParserContext
            Expr LiteralExpr StaticMethodExpr InstanceMethodExpr StaticPropertyExpr NumberExpr
            InstancePropertyExpr InstanceFieldExpr MapExpr VarExpr TheVarExpr InvokeExpr HostExpr
            FnExpr FnMethod BodyExpr LocalBindingExpr IfExpr VectorExpr NewExpr]
           [System.IO FileInfo Path]
           [System.Reflection TypeAttributes MethodAttributes FieldAttributes]
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

(defn load-argument [i]
  (cond
    (= i 0) (il/ldarg-0)
    (= i 1) (il/ldarg-1)
    (= i 2) (il/ldarg-2)
    (= i 3) (il/ldarg-3)
    :else (il/ldarg i)))

(def load-constant)

(defn box-if-value-type-value [a]
  (if (.IsValueType (type a))
    (il/box (type a))))

(defn box-if-value-type-expr [a]
  (if (and (.HasClrType a)
           (.IsValueType (.ClrType a)))
    (il/box (.ClrType a))))

(defn box-if-value-type [a]
  (cond
    (isa? (type a) Expr) (box-if-value-type-expr a)
    :else (box-if-value-type-value a)))

;; TODO overflows?
(def conv
  {Int32  (il/conv-i4)
   Int64  (il/conv-i8)
   Single (il/conv-r4)
   Double (il/conv-r8)})

(defn cast-if-different [obj dest-typ]
  (let [src-typ (if (and (isa? (type obj) Expr)
                          (.HasClrType obj))
                   (.ClrType obj)
                   (type obj))]
    (if (not (isa? src-typ dest-typ))
      (cond
        (and (.IsValueType src-typ)
             (.IsValueType dest-typ))
        (conv dest-typ)
        
        (and (.IsValueType src-typ) 
             (not (.IsValueType dest-typ)))
        [(il/box src-typ)
         (il/castclass dest-typ)]
         
        (and (not (.IsValueType src-typ))
             (.IsValueType dest-typ))
        (il/unbox-any dest-typ)
        
        (and (not (.IsValueType src-typ))
             (not (.IsValueType dest-typ)))
        (il/castclass dest-typ)))))

(defn load-vector
  ([v] (load-vector v load-constant))
  ([v f]
   [(load-constant (int (count v)))
    (il/newarr Object)
    (map (fn [i c]
           [(il/dup)
            (load-constant (int i))
            (f c)
            (box-if-value-type c)
            (il/stelem-ref)])
         (range)
         v)
    (il/call (find-method clojure.lang.RT "vector" |System.Object[]|))]))

(defn load-list
  ([v] (load-list v load-constant))
  ([v f]
   [(load-constant (int (count v)))
    (il/newarr Object)
    (map (fn [i c]
           [(il/dup)
            (load-constant (int i))
            (f c)
            (box-if-value-type c)
            (il/stelem-ref)])
         (range)
         v)
    (il/call (find-method clojure.lang.PersistentList "create" |System.Object[]|))]))

(defn load-map [keyvals]
  (let [ks (take-nth 2 keyvals)
        vs (take-nth 2 (drop 1 keyvals))]
    [(load-constant (int (+ (count ks) (count vs))))
     (il/newarr Object)
     (map (fn [i kv]
            [(il/dup)
             (load-constant (int i))
             (load-constant kv)
             ; (cast-if-different kv Object)
             (il/stelem-ref)])
          (range)
          (interleave ks vs))
     (il/call (find-method clojure.lang.PersistentArrayMap "createWithCheck" |System.Object[]|))]))

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
    (instance? clojure.lang.APersistentVector k)        (load-vector k)
    (instance? clojure.lang.APersistentMap k)           (load-map (seq k))))

(defn to-address [t]
  (let [l (il/local t)]
    [(il/stloc l)
     (il/ldloca l)]))

(defn cleanup-stack
  ([pcon]
   (if (.IsStatementContext pcon)
     (il/pop)))
  ([lasttype pcon]
   (if (and (not= System.Void lasttype)
            (.IsStatementContext pcon))
     (il/pop))))

(def intrinsics
  {(find-method clojure.lang.RT "uncheckedIntCast" Double)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "uncheckedIntCast" Int64)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "uncheckedIntCast" Single)
   (il/conv-i4)
   
   (find-method clojure.lang.RT "uncheckedIntCast" Int32)
   []
   
   (find-method clojure.lang.Numbers "unchecked_add" Double Int64)
   [(il/conv-r8)
    (il/add)]
      
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

;; ast -symbolize-> symbolics -emit-> bytecode
;;        M&GIC                M&GE

(def symbolize)

(def base-symbolizers
  {
   ;; 42
   ;; "foo"
   LiteralExpr (fn literal-symbolizer [this symbolizers]
                 (let [data (data-map this)]
                   (if-not (.IsStatementContext (data :ParsedContext))
                     (load-constant (data :Val)))))
   
   VectorExpr (fn vector-symbolizer [this symbolizers]
                (load-vector (-> this data-map :_args)
                             #(symbolize % symbolizers)))
   
   ;; {:foo bar}
   MapExpr (fn map-symbolizer [this symbolizers]
             (let [pcon (.ParsedContext this)
                   ks (take-nth 2 (.KeyVals this))
                   vs (take-nth 2 (drop 1 (.KeyVals this)))]
               [(load-constant (int (+ (count ks) (count vs))))
                (il/newarr Object)
                (map (fn [i kv]
                       [(il/dup)
                        (load-constant (int i))
                        (symbolize kv symbolizers)
                        (box-if-value-type kv)
                        (il/stelem-ref)])
                     (range)
                     (interleave ks vs))
                (il/call (find-method RT "mapUniqueKeys" |System.Object[]|))
                (cleanup-stack pcon)]))
   
   ;; (f a b)
   InvokeExpr (fn invoke-symbolizer [this symbolizers]
                (let [data (data-map this)
                      pcon (.ParsedContext this)
                      fexpr (:_fexpr data)
                      args (:_args data)
                      arity (count args)]
                  [(symbolize fexpr symbolizers)
                   (il/castclass IFn)
                   (map (fn [a]
                          [(symbolize a symbolizers)
                           (box-if-value-type a)])
                        args)
                   (il/callvirt (apply find-method IFn "invoke" (repeat arity Object)))
                   (cleanup-stack pcon)]))
   
   NewExpr (fn new-symbolizer [this symbolizers]
             (let [{:keys [_args _type _ctor] :as data} (data-map this)]
               (if _ctor
                 ;; have constructor, normal newobj path 
                 (let [arg-exprs (map #(.ArgExpr %) _args)
                       ctor-param-types (->> _ctor .GetParameters (map #(.ParameterType %)))]
                   ;; TODO what about LocalBindings?
                   [(interleave
                      (map #(symbolize % symbolizers)
                           arg-exprs)
                      (map #(cast-if-different %1 %2)
                           arg-exprs
                           ctor-param-types))
                    (il/newobj _ctor)])
                 ;; no constructor, might be initobj path
                 (if (.IsValueType _type)
                   (let [loc (il/local _type)]
                     [(il/ldloca-s loc)
                      (il/initobj _type)
                      (il/ldloc loc)])
                   (throw (Exception. (str "No constructor for non-valuetype " _type)))))))
   
   VarExpr (fn var-symbolizer [this symbolizers]
             (let [pcon (.ParsedContext this)
                   v (.. this Var)]
               [(load-var v)
                (get-var v)
                (cleanup-stack pcon)]))
   
   TheVarExpr (fn var-symbolizer [this symbolizers]
                (let [pcon (.ParsedContext this)
                      v (-> this data-map :_var)]
                  [(load-var v)
                   (get-var v)
                   (cleanup-stack pcon)]))
   
   ;; interop
   
   ;; (+ 1 2)
   ;; (Foo/Bar a b)
   StaticMethodExpr (fn static-method-symbolizer [this symbolizers]
                      (let [data (data-map this)
                            pcon (.ParsedContext this)
                            args (map data-map (:_args data))
                            method (:_method data)]
                        
                        [(->> args
                              (map :ArgExpr)
                              (map #(symbolize % symbolizers))) 
                         
                         (if-let [intrinsic-bytecode (intrinsics method)]
                           intrinsic-bytecode
                           (il/call method))
                         
                         (cleanup-stack (.ReturnType method) pcon)]))
   
   InstanceMethodExpr (fn instance-method-symbolizer [this symbolizers]
                        (let [data (data-map this)
                              pcon (.ParsedContext this)
                              target (:_target data)
                              target-type (-> target .ClrType)
                              args (map data-map (:_args data))
                              method (:_method data)]
                          
                          [(symbolize target symbolizers)
                           (if (.IsValueType target-type)
                             (to-address target-type))
                           (->> args
                                (map :ArgExpr)
                                (map #(symbolize % symbolizers))) 
                           (if (.IsValueType target-type)
                             (il/call method)
                             (il/callvirt method))
                           (cleanup-stack (.ReturnType method)
                                          pcon)]))
   
   StaticPropertyExpr (fn static-property-symbolizer [this symbolizers]
                        (let [pcon (.ParsedContext this)
                              return-type (.ClrType this)
                              getter (-> this data-map :_tinfo .GetGetMethod)]
                          [(il/call getter)
                           (cleanup-stack return-type pcon)]))
   
   InstancePropertyExpr (fn instance-property-symbolizer [this symbolizers]
                          (let [data (data-map this)
                                pcon (.ParsedContext this)
                                return-type (.ClrType this)
                                target (:_target data)
                                getter (-> data :_tinfo .GetGetMethod)]
                            [(symbolize target symbolizers)
                             (il/callvirt getter)
                             (cleanup-stack return-type pcon)]))
   
   InstanceFieldExpr (fn instance-field-symbolizer [this symbolizers]
                       (let [data (data-map this)
                             pcon (.ParsedContext this)
                             target (:_target data)
                             field (:_tinfo data)
                             return-type (.FieldType field)]
                         [(symbolize target symbolizers)
                          (il/ldfld field)
                          (cleanup-stack return-type pcon)]))
   
   FnExpr (fn fn-symbolizer [this symbolizers]
            (let [{:keys [Name Constants Vars ProtocolCallsites VarCallsites Keywords _methods] :as data} (data-map this)
                  protected-static (enum-or FieldAttributes/Static FieldAttributes/FamORAssem)
                  
                  arities (->> _methods
                               (map data-map)
                               (map :NumParams))
                  
                  ;; set up var fields
                  vars (keys Vars)
                  var-fields (->> vars
                                  (map #(il/field (type %)
                                                  protected-static
                                                  (gensym (str (.. % Symbol Name) "_"))))
                                  (interleave vars)
                                  (apply hash-map))
                  var-symbolizer (fn fn-specialized-var-symbolizer [this symbolizers]
                                   (let [pcon (.ParsedContext this)
                                         v (or (-> this data-map :_var)
                                               (.. this Var))]
                                     [(il/ldsfld (var-fields v))
                                      (get-var v)
                                      (cleanup-stack pcon)]))
                  symbolizers (assoc symbolizers
                                VarExpr var-symbolizer
                                TheVarExpr var-symbolizer)]
              (mage.core/type
                Name
                TypeAttributes/Public []
                clojure.lang.AFn
                
                [(il/constructor
                   MethodAttributes/Public
                   CallingConventions/Standard []
                   (il/ret))
                 (il/constructor
                   (enum-or MethodAttributes/Static)
                   CallingConventions/Standard []
                   [;; populate var fields
                    (map (fn [[v fld]] [(load-var v) (il/stsfld fld)])
                         var-fields)
                    (il/ret)])
                 (il/method
                   "HasArity"
                   (enum-or MethodAttributes/Public
                            MethodAttributes/Virtual)
                   Boolean [Int32]
                   (let [ret-true (il/label)]
                     [(map
                        (fn [arity]
                          [(il/ldarg-1)
                           (load-constant arity)
                           (il/beq ret-true)])
                        arities)
                      (il/ldc-i4-0)
                      (il/ret)
                      ret-true
                      (il/ldc-i4-1)
                      (il/ret)]))
                 (map #(symbolize % symbolizers) _methods)])
              ; data
              ))
   
   FnMethod (fn fn-method-symbolizer [this symbolizers]
              (let [{:keys [_retType _reqParms _body] :as data} (data-map this)]
                (il/method "invoke"
                           (enum-or MethodAttributes/Public
                                    MethodAttributes/Virtual)
                           _retType (mapv (constantly Object) _reqParms)
                           [(symbolize _body symbolizers)
                            (box-if-value-type (.LastExpr _body))
                           (il/ret)]
                           )))
   
   IfExpr (fn if-symbolizer [this symbolizers]
            (let [{:keys [_testExpr _thenExpr _elseExpr] :as data} (data-map this)
                  false-label (il/label)
                  true-label (il/label)
                  end-label (il/label)]
              [(symbolize _testExpr symbolizers)
               (il/brfalse false-label)
               (symbolize _thenExpr symbolizers)
               (il/br end-label)
               false-label
               (symbolize _elseExpr symbolizers)
               end-label]))
   
   BodyExpr (fn body-symbolizer [this symbolizers]
              (map #(symbolize % symbolizers) (-> this data-map :_exprs)))
   
   LocalBindingExpr (fn local-binding-symbolizer [this symbolizers]
                      (let [{:keys [IsArg Index ClrType]} (-> this data-map :Binding data-map)]
                        (if IsArg
                          (load-argument Index)
                          (il/local (or ClrType Object)) ;; ??
                          )))
   })

(defn ast->symbolizer [ast symbolizers]
  (or (->> ast type symbolizers)
      (->> ast
           type
           bases
           (map symbolizers)
           (remove nil?)
           first)))

(defn symbolize [ast symbolizers]
  (if-let [symbolizer (ast->symbolizer ast symbolizers)]
    (symbolizer ast symbolizers)))


(comment 
  (use 'clojure.pprint)
  (in-ns 'mage.core)
  
  ;; nested fns need mage work
  (-> (data-map (analyze '(fn [] (fn [a] (+ 1)))))
      :_methods
      first
      data-map
      :_body
      data-map
      :_exprs
      first
      data-map
      )
  
  (fn [] (fn [a] (+ a 1)))
  
  (fn []
    ((if (< 0.5 (rand)) #'+ #'-) 4 5))
  
  (-> (analyze '(if false #'+ #'-))
      data-map
      :_elseExpr)
  
    (->
      (il/assembly "magicTest"
                   (il/module "magicTest.dll"
                              (symbolize (analyze '(fn []
    ((if (< 0.5 (rand)) #'+ #'-) 4 5)))
                                         base-symbolizers)
                              ))
      il/emit!
      ; pprint
      )
    
  ((magic.core$prancer.))
  
  )

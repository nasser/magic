(ns magic.analyzer.types
  (:refer-clojure :exclude [resolve])
  (:require [magic.analyzer
             [binder :refer [select-method]]
             [util :refer [throw! var-interfaces var-type] :as util]
             [reflection :refer [find-method]]]
            [magic.emission :refer [*module*]])
  (:import Magic.Runtime
           [System Double Single 
            Int16 Int32 Int64
            UInt16 UInt32 UInt64
            IntPtr UIntPtr Char
            SByte Decimal]))

(defn superchain [t]
  (if-let [b (.BaseType t)]
    (cons b (superchain b))))

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

(def shorthand
  {"float"    Single
   "double"   Double
   "short"    Int16
   "ushort"   UInt16
   "int"      Int32
   "uint"     UInt32
   "long"     Int64
   "ulong"    UInt64
   "boolean"  Boolean
   "object"   Object
   "intptr"   IntPtr
   "uintptr"  UIntPtr
   "char"     Char
   "byte"     Byte
   "sbyte"    SByte
   "decimal"  Decimal
   "string"   String
   "floats"   System.Single|[]|
   "doubles"  System.Double|[]|
   "shorts"   System.Int16|[]|
   "ushorts"  System.UInt16|[]|
   "ints"     System.Int32|[]|
   "uints"    System.UInt32|[]|
   "longs"    System.Int64|[]|
   "ulongs"   System.UInt64|[]|
   "booleans" System.Boolean|[]|
   "objects"  System.Object|[]|
   "intptrs"  System.IntPtr|[]|
   "uintptrs" System.UIntPtr|[]|
   "chars"    System.Char|[]|
   "bytes"    System.Byte|[]|
   "sbytes"   System.SByte|[]|
   "decimals" System.Decimal|[]|
   "strings"  System.String|[]|})

(def cached-ns-imports ns-imports)

(defn resolve 
  ([t] (resolve t *ns*))
  ([t ns]
   (cond
     (nil? t)
     nil
     (instance? Type t)
     (recur (.FullName t) ns)
     (symbol? t)
     (recur (str t) ns)
     (string? t)
     (or (shorthand t)
         (and *module*
              (try 
                (let [name t
                      qualified-name (str (namespace-munge ns) "." name)]
                  (or (.GetType *module* qualified-name) (.GetType *module* name)))
                (catch ArgumentException e
                  nil)))
         (Runtime/FindType t)
         (and ns 
              (get (cached-ns-imports ns) (symbol t))))
     :else
     nil)))

(defn is-array? [t]
  (and (instance? Type t) (.IsArray t) (= 1 (.GetArrayRank t))))

(defn is-value-type? [t]
  (and (instance? Type t) (.IsValueType t)))

(defn is-enum? [t]
  (and (instance? Type t) (.IsEnum t)))

(defn is-primitive? [t]
  (and (instance? Type t) (.IsPrimitive t)))

(defn tag [x]
  (when-let [t (-> x meta :tag)]
    (resolve t)))

(defmulti ast-type-impl
  "The CLR type of an AST node"
  :op)

(defn ast-type-or-object [ast]
  (or (ast-type-impl ast)
      Object))

(defn disregard-type? [ast]
  (case (:op ast)
    :if (and (disregard-type? (:then ast))
             (disregard-type? (:else ast)))
    :let (disregard-type? (:body ast))
    :do (disregard-type? (:ret ast))
    (= ::disregard (ast-type-impl ast))))

;;;;; intrinsics

;; TODO this should be part of a more general conversion system
;; TODO Boolean? Decimal?
(def numeric-promotion-order
  [Byte SByte UInt16 Int16 UInt32 Int32 UInt64 Int64 Single Double])

(def numeric
  (set numeric-promotion-order))

(defn numeric-type? [t]
  (numeric t))

(def integer
  (disj numeric Single Double))

(defn integer-type? [t]
  (integer t))


(defn best-numeric-promotion [types]
  (and (or (every? numeric types) nil)
       (->> types
            (sort-by #(.IndexOf numeric-promotion-order %))
            last)))

;;;;; ast types

;; :forward ast -> type
;; :reverse type name -> ast
(def type-lookup-cache (atom {:forward {} :reverse {}}))

(defn type-lookup-cache-store [cache ast type]
  (cond
    (instance? Type type)
    (-> cache
        (update :forward assoc ast type)
        (update :reverse assoc (.FullName type) ast))
    :else
    (-> cache
        (update :forward assoc ast type))))

(defn type-lookup-cache-evict [cache type-name]
  (if-let [ast (get (:reverse cache) type-name)]
    (-> cache
        (update :forward dissoc ast)
        (update :reverse dissoc type-name))
    cache))

(defn type-lookup-cache-evict! [type-name]
  (swap! type-lookup-cache type-lookup-cache-evict type-name))

(defn ast-type-impl* [ast]
  (if-let [cached-type (get (:forward @type-lookup-cache) ast)]
    cached-type
    (let [type (ast-type-impl ast)]
      (swap! type-lookup-cache type-lookup-cache-store ast type)
      type)))

(defn ast-type [ast]
  (if-let [tag (-> ast :form meta :tag)]
    (resolve tag)
    (ast-type-impl* ast)))

(defn ast-type-ignore-tag [ast]
  (ast-type-impl* ast))

(defn non-void-ast-type
  ([ast] (non-void-ast-type ast Object))
  ([ast non-void-type]
   (if (disregard-type? ast)
     non-void-type
     (let [t (ast-type ast)]
       (if (= t System.Void)
         non-void-type
         t)))))

(defmethod ast-type-impl :default [ast]
  nil
  #_(throw! "ast-type-impl not implemented for :op " (:op ast) " while analyzing " (or (-> ast :raw-forms first pr-str)
                                                                                     (:form ast))))

(defmethod ast-type-impl :fn
  [{:keys [fn-type]}] clojure.lang.IFn #_fn-type)

(defmethod ast-type-impl :proxy
  [{:keys [proxy-type]}] proxy-type)

(defmethod ast-type-impl :reify
  [{:keys [reify-type]}] reify-type)

(defmethod ast-type-impl :tagged [{:keys [tag]}] (resolve tag))

(defmethod ast-type-impl :gen-interface [_] System.Type)

(defmethod ast-type-impl :deftype [_] System.Type)

(defmethod ast-type-impl :import [_] System.Type)

(defmethod ast-type-impl :dynamic-constructor [{:keys [type]}] type)

(defmethod ast-type-impl :dynamic-zero-arity [_] Object)

(defmethod ast-type-impl :dynamic-instance-method [_] Object)

(defmethod ast-type-impl :dynamic-static-method [_] Object)

(defmethod ast-type-impl :intrinsic
  [{:keys [type]}] type)

(defmethod ast-type-impl :def
  [{:keys [type]}] clojure.lang.Var)

;; TODO remove
(defmethod ast-type-impl nil
  [ast]
  Object
  #_
  (throw! "ast-type-impl not implemented for nil"))

(defmethod ast-type-impl :do
  [{:keys [ret] :as ast}]
  (ast-type-impl ret))

(defmethod ast-type-impl :case
  [{:keys [expressions default] :as ast}]
  (let [result-types (-> #{(ast-type-impl default)}
                         (into (map ast-type-impl expressions))
                         (disj :magic.analyzer.types/disregard))]
    (if (= 1 (count result-types))
      (first result-types)
      Object)))

(defmethod ast-type-impl :set!
  [{:keys [val] {:keys [context]} :env}]
  (if (= context :ctx/statement)
    System.Void
    (ast-type-impl val)))

(def data-structure-types
  {:seq clojure.lang.IPersistentList
   :vector clojure.lang.APersistentVector
   :set clojure.lang.APersistentSet
   :map clojure.lang.APersistentMap})

(defmethod ast-type-impl :quote
  [{:keys [expr] :as ast}]
  (or 
   (data-structure-types (:type expr))
   (ast-type-impl expr)))

(defmethod ast-type-impl :maybe-class
  [{:keys [class] :as ast}]
  (resolve class ast))

(defmethod ast-type-impl :var [ast]
  Object)

(defmethod ast-type-impl :the-var [ast]
  Object)

(defmethod ast-type-impl :const [ast]
  (if (= :class (:type ast))
    System.Type ;; (:val ast) ; oh jesus
    (or (type (:val ast)) Object))) ;; nil -> Object

(defmethod ast-type-impl :vector [ast]
  clojure.lang.APersistentVector)

(defmethod ast-type-impl :set [ast]
  clojure.lang.APersistentSet)

(defmethod ast-type-impl :map [ast]
  clojure.lang.APersistentMap)

(defmethod ast-type-impl :with-meta [{:keys [expr]}]
  clojure.lang.IObj) ;; ??

(defmethod ast-type-impl :static-method [ast]
  (-> ast :method .ReturnType))

(defmethod ast-type-impl :instance-method [ast]
  (-> ast :method .ReturnType))

(defmethod ast-type-impl :static-property [ast]
  (-> ast :property .PropertyType))

(defmethod ast-type-impl :instance-property [ast]
  (-> ast :property .PropertyType))

(defmethod ast-type-impl :static-field [ast]
  (-> ast :field .FieldType))

(defmethod ast-type-impl :instance-field [ast]
  (-> ast :field .FieldType))

;; TODO dynamics always typed as object?
(defmethod ast-type-impl :dynamic-field [ast]
  System.Object)

(defmethod ast-type-impl :invoke
  [{:keys [fn args] :as ast}]
  (resolve
   (or (->> ast
            :meta
            :tag)
       (->> fn
            :meta
            :tag)
       (->> fn
            :meta
            :arglists
            (filter #(= (count %) (count args)))
            first
            meta
            :tag)
       ;; TODO revisit high performance generic function interfaces
       #_(let [arg-types (map ast-type-impl args)
               target-interfaces (var-interfaces fn)
              ;; TODO this is hacky and gross
               vt (var-type fn)
               invokes (when vt
                         (filter #(= (.Name %) "invokeTyped")
                                 (.GetMethods vt)))
               exact-match (when invokes
                             (select-method invokes arg-types))]
           (if exact-match
             (.ReturnType exact-match)
             Object))
       Object)))

(defmethod ast-type-impl :new [ast]
  (:type ast))

(defmethod ast-type-impl :initobj [ast]
  (:type ast))

(defmethod ast-type-impl :maybe-host-form
  [ast]
  (throw! "Trying to find type of :maybe-host-form in " ast))

(defmethod ast-type-impl :host-interop
  [ast]
  (throw! "Trying to find type of :host-interop in " ast))

;; TODO -> form locals :form meta :tag ??
(defmethod ast-type-impl :binding [ast]
  (or
   (when-let [tag (-> ast :form meta :tag)]
     (resolve tag))
   (when-let [init (:init ast)]
     (ast-type-impl init))
   Object))

(defmethod ast-type-impl :local
  [{:keys [name form local by-ref?] {:keys [locals]} :env :as ast}]
  (let [tag (-> form locals :form meta :tag)
        type (cond tag
                   (if-let [t (resolve tag)]
                     t
                     (throw! "Could not resolve type hint " tag " while analyzing form " form))
                   (= local :arg)
                   Object
                   (= local :proxy-this)
                   (or (:proxy-type ast) Object)
                   :else
                   (non-void-ast-type (-> form locals :init)))]
    (if by-ref?
      (.MakeByRefType type)
      type)))

(defmethod ast-type-impl :let [ast]
  (-> ast :body ast-type-impl))

(defmethod ast-type-impl :loop [ast]
  (-> ast :body ast-type-impl))

(defmethod ast-type-impl :letfn [ast]
  (-> ast :body ast-type-impl))

;; ???
(defmethod ast-type-impl :recur [ast]
  ::disregard)

(defmethod ast-type-impl :throw [ast]
  ::disregard)

(defmethod ast-type-impl :try [{:keys [body catches] :as ast}]
  (let [body-type (ast-type-impl body)
        catches-types (->> catches
                           (remove disregard-type?)
                           (map ast-type-impl)
                           (into #{}))]
    (if (or (empty? catches-types)
            (and (= 1 (count catches-types))
                 (= body-type (first catches-types))))
      body-type
      Object)))

(defmethod ast-type-impl :catch [{:keys [body] :as ast}]
  (ast-type-impl body))

(defmethod ast-type-impl :monitor-enter [ast]
  System.Void)

(defmethod ast-type-impl :monitor-exit [ast]
  System.Void)

(defn always-then?
  "Is an if AST node always true?"
  [{:keys [op test then else]}]
  (and (= op :if)
       (= :const (:op test))
       (or (and (:literal? test)
                (not= (:val test) false)
                (not= (:val test) nil))
           (and (is-value-type? (ast-type-impl test))
                (not= Boolean (ast-type-impl test))))))

(defn always-else?
  "Is an if AST node always false?"
  [{:keys [op test then else]}]
  (and (= op :if)
       (= :const (:op test))
       (:literal? test)
       (or (= (:val test) false)
           (= (:val test) nil))))

(defmethod ast-type-impl :if
  [{:keys [form test then else] :as ast}]
  (if-let [t (tag form)]
    t
    (let [then-type (ast-type-impl then)
          else-type (ast-type-impl else)]
      (cond
        (= then-type else-type) then-type
        (always-then? ast) then-type
        (always-else? ast) else-type
        (disregard-type? then) else-type
        (disregard-type? else) then-type
        ;; TODO compute common type  
        :else Object))))

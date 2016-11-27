(ns magic.core
  (:refer-clojure :exclude [compile resolve])
  (:require [mage.core :as il]
            [magic.analyzer :as ana]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang RT]
           [System.IO FileInfo Path]
           [System.Reflection TypeAttributes MethodAttributes FieldAttributes FieldInfo MethodInfo PropertyInfo]
           System.AppDomain))

(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))

(defmulti load-constant type)

(defn load-argument [i]
  (cond
    (= i 0) (il/ldarg-1)
    (= i 1) (il/ldarg-2)
    (= i 2) (il/ldarg-3)
    :else (il/ldarg i)))

(defmethod load-constant :default [k]
  (throw! "load-constant not implemented for " (type k)))

(defmethod load-constant String [k]
  (il/ldstr k))

(defmethod load-constant Int32 [k]
  (il/ldc-i4 k))

(defmethod load-constant Int64 [k]
  (il/ldc-i8 k))

;;; types
(defmulti clr-type :op)

(defmethod clr-type :default [ast]
  (throw! "clr-type not implemented for " (pr-str ast)))

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
      (interop/method from-type "op_Implicit" to)
      (il/call (interop/method from-type "op_Implicit" to))
      
      ;; use user defined explicit conversion if it exists
      (interop/method from-type "op_Explicit" to)
      (il/call (interop/method from-type "op_Explicit" to))
      
      ;; use intrinsic conv opcodes from primitive to primitive
      (and (.IsPrimitive from-type) (.IsPrimitive to))
      (intrinsic-conv to)
      
      ;; box valuetypes to objects
      (and (.IsValueType from-type) (= to Object))
      (il/box from-type)
      
      ;; RT casts
      (and (= from-type Object) (= to Single))
      (il/call (interop/method RT "floatCast" from-type))
      (and (= from-type Object) (= to Double))
      (il/call (interop/method RT "doubleCast" from-type))
      (and (= from-type Object) (= to Int32))
      (il/call (interop/method RT "intCast" from-type))
      (and (= from-type Object) (= to Int64))
      (il/call (interop/method RT "longCast" from-type))
      
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

(defmethod clr-type :const [ast]
  (-> ast :val type))

(defn const-symbolizer
  "Symbolic bytecode for :const nodes"
  [{:keys [val] :as ast}
   symbolizers]
  [(load-constant val)
   (cleanup-stack ast)])

(defmethod clr-type :fn [ast]
  (-> ast :val type))

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

(defn fn-symbolizer
  [{:keys [methods raw-forms] :as ast} symbolizers]
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
         [(let [form-field (il/field Object FieldAttributes/Public "originalForm")]
            (il/ldstr (-> raw-forms first str))
            (il/stfld form-field))
          (il/ret)])
       (has-arity-method arities)
       (map #(symbolize % symbolizers) methods)])))

(defn fn-method-symbolizer
  [{:keys [body params] {:keys [ret statements]} :body} symbolizers]
  (il/method "invoke"
             (enum-or MethodAttributes/Public
                      MethodAttributes/Virtual)
             ;; Object (mapv (constantly Object) params)
             (clr-type ret) (mapv clr-type params)
             [(symbolize body symbolizers)
              (convert ret Object)
              (il/ret)]))


(def base-symbolizers
  {:const           #'const-symbolizer
   :do              #'do-symbolizer
   :fn              #'fn-symbolizer
   :fn-method       #'fn-method-symbolizer})

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
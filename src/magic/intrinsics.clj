(ns magic.intrinsics
  (:require [mage.core :as il]
            [magic.core :as magic]
            [magic.analyzer :as ana]
            [magic.analyzer.literal-reinterpretation :refer [reinterpret-value reinterpret]]
            [magic.analyzer.intrinsics :as intrinsics :refer [register-intrinsic-form]]
            [magic.analyzer.types :as types :refer [tag ast-type non-void-ast-type]]
            [magic.interop :as interop]
            [clojure.string :as string])
  (:import [clojure.lang RT BigInteger Numbers Ratio]))

(defmacro defintrinsic [name type-fn il-fn]
  `(register-intrinsic-form
     '~name
     ~type-fn
     ~il-fn))

(defn numeric-args [{:keys [args]}]
  (let [arg-types (->> args (map ast-type))
        non-numeric-args (filter (complement types/numeric) arg-types)]
    (when (empty non-numeric-args)
      (types/best-numeric-promotion arg-types))))

(defn best-numeric-type [{:keys [args]}]
  (let [arg-types (->> args (map ast-type))
        non-numeric-args (filter (complement types/numeric) arg-types)
        inline? (empty? non-numeric-args)
        type (->> args (map ast-type) types/best-numeric-promotion)]
    (when inline? type)))

(defn add-mul-numeric-type [{:keys [args] :as ast}]
  (if (empty? args)
    Int64
    (numeric-args ast)))

(defn numeric-arg [{:keys [args]}]
  (let [arg-type (-> args first ast-type)]
    (when (types/numeric arg-type)
      arg-type)))

(defn when-numeric-arg [{:keys [args]} x]
  (let [arg-type (-> args first ast-type)]
    (when (types/numeric arg-type)
      x)))

(defn add-mul-numeric-compiler [ident checked unchecked]
  (fn add-mul-compiler
    [{:keys [args] :as ast} type compilers]
    (if (zero? (count args))
      ident
      [(interleave
         (map #(magic/compile (reinterpret % type) compilers) args)
         (map #(magic/convert (ast-type %) type) args))
       (repeat (-> args count dec)
               (if (not (or *unchecked-math*
                            (= type Single)
                            (= type Double)))
                 checked
                 unchecked))])))

(defn conversion-compiler
  [{:keys [args]} type compilers]
  (let [arg (reinterpret (first args) type)]
    [(magic/compile arg compilers)
     (magic/convert (ast-type arg) type)]))

(def conversions
  {'clojure.core/float  Single
   'clojure.core/double Double
   'clojure.core/char   Char
   'clojure.core/byte   Byte
   'clojure.core/sbyte  SByte
   'clojure.core/int    Int32
   'clojure.core/uint   UInt32
   'clojure.core/long   Int64
   'clojure.core/ulong  UInt64
   'clojure.core/short  Int16
   'clojure.core/ushort UInt16})

(reduce-kv
  #(register-intrinsic-form
     %2
     (constantly %3)
     conversion-compiler)
  nil
  conversions)

(defintrinsic clojure.core/+
  add-mul-numeric-type
  (add-mul-numeric-compiler
    (il/ldc-i8 0) (il/add-ovf) (il/add)))

(defintrinsic clojure.core/inc
  add-mul-numeric-type
  (fn intrinsic-inc-compiler
    [{:keys [args] :as ast} type compilers]
    (let [arg (first args)]
      [(magic/compile arg compilers)
       (magic/load-constant
         (reinterpret-value 1 type))
       (if *unchecked-math*
         (il/add)
         (il/add-ovf))])))

(defintrinsic clojure.core/-
  best-numeric-type
  (fn intrinsics-sub-compiler
    [{:keys [args] :as ast} type compilers]
    (let [first-arg (first args)
          rest-args (rest args)
          instr (if (not (or *unchecked-math*
                             (= type Single)
                             (= type Double)))
                  (il/sub-ovf)
                  (il/sub))]
      (if (empty? rest-args)
        [(magic/compile (reinterpret first-arg type) compilers)
         (magic/convert (ast-type first-arg) type)
         (il/neg)]
        [(magic/compile (reinterpret first-arg type) compilers)
         (magic/convert (ast-type first-arg) type)
         (mapcat
           (fn [a]
             [(magic/compile (reinterpret a type) compilers)
              (magic/convert (ast-type a) type)
              instr])
           rest-args)]))))

(defintrinsic clojure.core/dec
  best-numeric-type
  (fn intrinsic-dec-compiler
    [{:keys [args] :as ast} type compilers]
    (let [arg (first args)]
      [(magic/compile arg compilers)
       (magic/load-constant
         (reinterpret-value 1 type))
       (if *unchecked-math*
         (il/sub)
         (il/sub-ovf))])))

(defintrinsic clojure.core/*
  add-mul-numeric-type
  (add-mul-numeric-compiler
    (il/ldc-i8 1) (il/mul-ovf) (il/mul)))

(defintrinsic clojure.core//
  #(let [t (best-numeric-type %)]
     (if (types/integer t)
       Ratio
       t))
  (fn intrinsics-div-compiler
    [{:keys [args] :as ast} type compilers]
    (let [first-arg (first args)
          first-type (ast-type first-arg)
          rest-args (rest args)]
      (cond
        (and (empty? rest-args)
             (types/integer first-type))
        [(il/call (interop/getter BigInteger "One"))
         (magic/compile first-arg compilers)
         (magic/convert first-type BigInteger)
         (il/newobj (interop/constructor Ratio BigInteger BigInteger))]
        (empty? rest-args)
        [(magic/load-constant
           (reinterpret-value 1 first-type))
         (magic/compile first-arg compilers)
         (il/div)]
        (= Ratio type)
        (let [second-arg (second args)
              second-type (ast-type second-arg)
              rest-args (drop 2 args)]
          [(magic/compile first-arg compilers)
           (magic/convert first-type BigInteger)
           (magic/compile second-arg compilers)
           (magic/convert second-type BigInteger)
           (mapcat
             (fn [a]
               [(magic/compile a compilers)
                (magic/convert (ast-type a) BigInteger)
                (il/call (interop/method BigInteger "op_Multiply" BigInteger BigInteger))])
             rest-args)
           (il/call (interop/method Numbers "BIDivide" BigInteger BigInteger))
           (magic/convert Object Ratio)])
        :else
        [(magic/compile (reinterpret first-arg type) compilers)
         (magic/convert first-type type)
         (mapcat
           (fn [a]
             [(magic/compile (reinterpret a type) compilers)
              (magic/convert (ast-type a) type)
              (il/div)])
           rest-args)]))))

(defintrinsic clojure.core/<
  #(when (numeric-args %) Boolean)
  (fn intrinsic-lt-compiler
    [{:keys [args] :as ast} type compilers]
    (let [arg-pairs (partition 2 1 args)
          greater-label (il/label)
          true-label (il/label)
          end-label (il/label)
          il-pairs
          (map (fn [[a b]]
                 (let [best-numeric-type
                       (->> [a b] (map ast-type) types/best-numeric-promotion)]
                   [(magic/compile a compilers)
                    (magic/convert (ast-type a) best-numeric-type)
                    (magic/compile b compilers)
                    (magic/convert (ast-type b) best-numeric-type)]))
               arg-pairs)]
      [(->> (interleave il-pairs (repeat (il/bge greater-label)))
            drop-last)
       (il/clt)
       (il/br end-label)
       greater-label
       (il/ldc-i4-0)
       end-label])))

(defn array-type [{:keys [args]}]
  (let [type (-> args first ast-type)]
    (when (.IsArray type) type)))

(defn when-array-type [{:keys [args]} v]
  (let [type (-> args first ast-type)]
    (when (.IsArray type) v)))

(defn array-element-type [{:keys [args]}]
  (let [type (-> args first ast-type)]
    (when (.IsArray type)
      (.GetElementType type))))

(defintrinsic clojure.core/aclone
  array-type
  (fn intrinsic-aclone-compiler
    [{:keys [args] :as ast} type compilers]
    [(magic/compile (first args) compilers)
     (il/callvirt (interop/method type "Clone"))
     (magic/convert Object type)]))

;; TODO multidim arrays
(defintrinsic clojure.core/aget
  array-element-type
  (fn intrinsic-aget-compiler
    [{:keys [args] :as ast} type compilers]
    (let [[array-arg index-arg] args
          index-arg (reinterpret index-arg Int32)]
      [(magic/compile array-arg compilers)
       (magic/compile index-arg compilers)
       ;; TODO make sure this is conv.ovf
       (magic/convert (ast-type index-arg) Int32)
       (magic/load-element type)])))

;; TODO multidim arrays
(defintrinsic clojure.core/aset
  (fn [ast] 
    (if (magic/statement? ast)
      System.Void
      (array-element-type ast)))
  (fn intrinsic-aget-compiler
    [{:keys [args] :as ast} type compilers]
    (let [[array-arg index-arg value-arg] args
          index-arg (reinterpret index-arg Int32)
          type (array-element-type ast)
          val-return (il/local type)
          statement? (magic/statement? ast)]
      [(magic/compile array-arg compilers)
       (magic/compile index-arg compilers)
       ;; TODO make sure this is conv.ovf
       (magic/convert (ast-type index-arg) Int32)
       (magic/compile value-arg compilers)
       (magic/convert (ast-type value-arg) type)
       (when-not statement?
         [(il/dup)
          (il/stloc val-return)])
       (magic/store-element type)
       (when-not statement?
         (il/ldloc val-return))])))

(defintrinsic clojure.core/alength
  #(when-array-type % Int32)
  (fn intrinsic-alength-compiler
    [{:keys [args] :as ast} type compilers]
    [(magic/compile (first args) compilers)
     (il/ldlen)]))

(defintrinsic clojure.core/unchecked-inc
  numeric-arg
  (fn intrinsic-alength-compiler
    [{:keys [args] :as ast} type compilers]
    [(magic/compile (first args) compilers)
     (il/ldc-i4-1)
     (magic/convert Int32 type)
     (il/add)]))

(defintrinsic clojure.core/unchecked-inc-int
  #(when-numeric-arg % Int32)
  (fn intrinsic-alength-compiler
    [{:keys [args] :as ast} type compilers]
    [(magic/compile (first args) compilers)
     (magic/convert (ast-type (first args)) Int32)
     (il/ldc-i4-1)
     (il/add)]))

(defintrinsic clojure.core/instance?
  (fn [{[first-arg] :args}]
    (when (and (= :const (:op first-arg))
               (= :class (:type first-arg)))
      Boolean))
  (fn intrinsic-instance?-compiler
    [{[{:keys [val] :as type-arg} obj-arg] :args} type compilers]
    [(magic/compile obj-arg compilers)
     (il/isinst val)
     (il/ldnull)
     (il/cgt-un)]))

(defintrinsic clojure.core/count
  (constantly Int32)
  (fn intrinsic-count-compiler
    [{[first-arg] :args} type compilers]
    (let [arg-type (ast-type first-arg)]
      [(magic/compile first-arg compilers)
       (if (.IsArray arg-type)
         (il/ldlen)
         [(magic/convert arg-type Object)
          (il/call (interop/method RT "count" Object))]
         )])))

(defintrinsic clojure.core/make-array
  (fn [{[first-arg] :args}]
    (when (and (= :const (:op first-arg))
               (= :class (:type first-arg)))
      (.MakeArrayType (:val first-arg))))
  (fn intrinsic-make-array-compiler
    [{[type-arg len-arg] :args} type compilers]
    [(magic/compile len-arg compilers)
     (il/newarr (:val type-arg))]))

;;;; array functions
;; aclone
;; aget
;; alength
;; amap
;; areduce
;; aset
;; aset-*
;; *-array
;; into-array
;; nth
;; to-array
;; to-array-2d
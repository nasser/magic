(ns magic.intrinsics
  (:require [mage.core :as il]
            [magic.core :as magic]
            [magic.analyzer :as ana]
            [magic.analyzer.intrinsics :as intrinsics :refer [register-intrinsic-form]]
            [magic.analyzer.types :as types :refer [tag clr-type non-void-clr-type best-match]]
            [magic.interop :as interop]
            [clojure.string :as string]))

(defmacro defintrinsic [name type-fn il-fn]
  `(register-intrinsic-form
     '~name
     ~type-fn
     ~il-fn))

(defn associative-numeric-type [{:keys [args]}]
  (let [arg-types (->> args (map clr-type))
        non-numeric-args (filter (complement types/numeric) arg-types)
        inline? (when-not (some #{Object} arg-types)
                  (empty? non-numeric-args))
        type (case (-> args count)
               0 Int64
               1 (clr-type (first args))
               (->> args (map clr-type) types/best-numeric-promotion))]
    (when inline? type)))

(defn numeric-args [{:keys [args]}]
  (let [arg-types (->> args (map clr-type))
        non-numeric-args (filter (complement types/numeric) arg-types)]
    (when (empty non-numeric-args)
      (types/best-numeric-promotion arg-types))))

(defn numeric-arg [{:keys [args]}]
  (let [arg-type (-> args first clr-type)]
    (when (types/numeric arg-type)
      arg-type)))

(defn when-numeric-arg [{:keys [args]} x]
  (let [arg-type (-> args first clr-type)]
    (when (types/numeric arg-type)
      x)))

(defn associative-numeric-compiler [ident checked unchecked]
  (fn associative-compiler
    [{:keys [args] :as ast} type compilers]
    (if (zero? (count args))
      ident
      [(interleave
         (map #(magic/compile % compilers) args)
         (map #(magic/convert (clr-type %) type) args))
       (repeat (-> args count dec)
               (if (not (or *unchecked-math*
                            (= type Single)
                            (= type Double)))
                 checked
                 unchecked))])))

(defn conversion-compiler
  [{:keys [args]} type compilers]
  (let [arg (first args)]
    [(magic/compile arg compilers)
     (magic/convert (clr-type arg) type)]))

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
  associative-numeric-type
  (associative-numeric-compiler
    (il/ldc-i8 0) (il/add-ovf) (il/add)))

(defintrinsic clojure.core/inc
  associative-numeric-type
  (fn intrinsic-inc-compiler
    [{:keys [args] :as ast} type compilers]
    (let [arg (first args)]
      [(magic/compile arg compilers)
       (magic/load-constant 1)
       (if *unchecked-math*
         (il/add)
         (il/add-ovf))])))

(defintrinsic clojure.core/*
  associative-numeric-type
  (associative-numeric-compiler
    (il/ldc-i8 1) (il/mul-ovf) (il/mul)))

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
                       (->> [a b] (map clr-type) types/best-numeric-promotion)]
                   [(magic/compile a compilers)
                    (magic/convert (clr-type a) best-numeric-type)
                    (magic/compile b compilers)
                    (magic/convert (clr-type b) best-numeric-type)]))
               arg-pairs)]
      [(->> (interleave il-pairs (repeat (il/bge greater-label)))
            drop-last)
       (il/clt)
       (il/br end-label)
       greater-label
       (il/ldc-i4-0)
       end-label])))

(defn array-type [{:keys [args]}]
  (let [type (-> args first clr-type)]
    (when (.IsArray type) type)))

(defn when-array-type [{:keys [args]} v]
  (let [type (-> args first clr-type)]
    (when (.IsArray type) v)))

(defn array-element-type [{:keys [args]}]
  (let [type (-> args first clr-type)]
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
    (let [[array-arg index-arg] args]
      [(magic/compile array-arg compilers)
       (magic/compile index-arg compilers)
       ;; TODO make sure this is conv.ovf
       (magic/convert (clr-type index-arg) Int32)
       (magic/load-element type)])))

;; TODO multidim arrays
(defintrinsic clojure.core/aset
  array-element-type
  (fn intrinsic-aget-compiler
    [{:keys [args] :as ast} type compilers]
    (let [[array-arg index-arg value-arg] args
          val-return (il/local type)
          statement? (magic/statement? ast)]
      [(magic/compile array-arg compilers)
       (magic/compile index-arg compilers)
       ;; TODO make sure this is conv.ovf
       (magic/convert (clr-type index-arg) Int32)
       (magic/compile value-arg compilers)
       (magic/convert (clr-type value-arg) type)
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
     (magic/convert (clr-type (first args)) Int32)
     (il/ldc-i4-1)
     (il/add)]))


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
;; make-array
;; nth
;; to-array
;; to-array-2d
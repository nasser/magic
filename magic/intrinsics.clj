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

(defn associative-numeric-symbolizer [ident checked unchecked]
  (fn associative-symbolizer
    [{:keys [args] :as ast} type symbolizers]
    (if (zero? (count args))
      ident
      [(interleave
         (map #(magic/symbolize % symbolizers) args)
         (map #(magic/convert (clr-type %) type) args))
       (repeat (-> args count dec)
               (if (not (or *unchecked-math*
                            (= type Single)
                            (= type Double)))
                 checked
                 unchecked))])))

(defn conversion-symbolizer
  [{:keys [args]} type symbolizers]
  (let [arg (first args)]
    [(magic/symbolize arg symbolizers)
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
     conversion-symbolizer)
  nil
  conversions)

(defintrinsic clojure.core/+
  associative-numeric-type
  (associative-numeric-symbolizer
    (il/ldc-i8 0) (il/add-ovf) (il/add)))

(defintrinsic clojure.core/*
  associative-numeric-type
  (associative-numeric-symbolizer
    (il/ldc-i8 1) (il/mul-ovf) (il/mul)))

(defintrinsic clojure.core/float
  (constantly Single)
  (fn [{:keys [args]} type symbolizers]
    [(magic/symbolize (first args) symbolizers)
     (magic/convert (clr-type (first args)) type)]))

(defintrinsic clojure.core/<
  #(when (numeric-args %) Boolean)
  (fn intrinsic-lt-symbolizer
    [{:keys [args] :as ast} type symbolizers]
    (let [arg-pairs (partition 2 1 args)
          greater-label (il/label)
          true-label (il/label)
          end-label (il/label)
          il-pairs
          (map (fn [[a b]]
                 (let [best-numeric-type
                       (->> [a b] (map clr-type) types/best-numeric-promotion)]
                   [(magic/symbolize a symbolizers)
                    (magic/convert (clr-type a) best-numeric-type)
                    (magic/symbolize b symbolizers)
                    (magic/convert (clr-type b) best-numeric-type)]))
               arg-pairs)]
      [(->> (interleave il-pairs (repeat (il/bge greater-label)))
            drop-last)
       (il/clt)
       (il/br end-label)
       greater-label
       (il/ldc-i4-0)
       end-label])))

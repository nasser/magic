(ns magic.spells.intrinsics
  (:require [magic.core :as magic]
            [magic.analyzer.types :refer [clr-type]]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [clojure.lang RT Numbers]))

(defn compare-and-return [t cmp]
  (let [a (il/local t)
        b (il/local t)
        a-true (il/label)
        end (il/label)]
    [(il/stloc b)
     (il/stloc a)
     (il/ldloc a)
     (il/ldloc b)
     cmp
     (il/brtrue a-true)
     (il/ldloc b)
     (il/br end)
     a-true
     (il/ldloc a)
     end]))

(def =false [(il/ldc-i4-0) (il/ceq)])
(def lte [(il/cgt) =false])
(def gte [(il/clt) =false])

(defn pop-convert [t il]
  (let [l (il/local t)]
    [(il/stloc l)
     (magic/intrinsic-conv t)
     (il/ldloc l)
     il]))

(def intrinsic-map
  {(interop/method Numbers "lte" Int64 Int64)
   lte
   
   (interop/method Numbers "lte" Double Double)
   lte
   
   (interop/method Numbers "lte" Double Int64)
   [(il/conv-r8)
    lte]
   
   (interop/method Numbers "lte" Int64 Double)
   (pop-convert Double lte)
   
   (interop/method Numbers "lt" Int64 Int64)
   (il/clt)
   
   (interop/method Numbers "lt" Double Double)
   (il/clt)
   
   (interop/method Numbers "lt" Double Int64)
   [(il/conv-r8)
    (il/clt)]
   
   (interop/method Numbers "lt" Int64 Double)
   (pop-convert Double (il/clt))
   
   (interop/method Numbers "gte" Int64 Int64)
   gte
   
   (interop/method Numbers "gte" Double Double)
   gte
   
   (interop/method Numbers "gte" Double Int64)
   [(il/conv-r8)
    gte]
   
   (interop/method Numbers "gte" Int64 Double)
   (pop-convert Double gte)
   
   (interop/method Numbers "gt" Int64 Int64)
   (il/cgt)
   
   (interop/method Numbers "gt" Double Double)
   (il/cgt)
   
   (interop/method Numbers "gt" Double Int64)
   [(il/conv-r8)
    (il/cgt)]
   
   (interop/method Numbers "gt" Int64 Double)
   (pop-convert Double (il/cgt))
   
   (interop/method Numbers "isNeg" Double)
   [(il/ldc-r8 0.0)
    (il/clt)]
   
   (interop/method Numbers "isPos" Double)
   [(il/ldc-r8 0.0)
    (il/cgt)]
   
   (interop/method Numbers "isNeg" Int64)
   [(il/ldc-i8 0)
    (il/clt)]
   
   (interop/method Numbers "isPos" Int64)
   [(il/ldc-i8 0)
    (il/cgt)]
   
   (interop/method Numbers "max" Double Double)
   (compare-and-return Double (il/cgt))
   
   (interop/method Numbers "max" Int64 Int64)
   (compare-and-return Int64 (il/cgt))
   
   ;; TODO why does this work? copied from C# disasm
   (interop/method Numbers "shiftLeft" Int64 Int32)
   [(il/ldc-i4-s (byte 63)) 
    (il/and)
    (il/shl)]
    
   (interop/method Numbers "shiftLeft" Int64 Int64)
   [(il/conv-i4)
    (il/ldc-i4-s (byte 63)) 
    (il/and)
    (il/shl)]
    
   (interop/method Numbers "shiftRight" Int64 Int32)
   [(il/ldc-i4-s (byte 63)) 
    (il/and)
    (il/shr)]
    
   (interop/method Numbers "shiftRight" Int64 Int64)
   [(il/conv-i4)
    (il/ldc-i4-s (byte 63)) 
    (il/and)
    (il/shr)]
    
   (interop/method Numbers "isZero" Int64)
   [(il/ldc-i8 0) 
    (il/ceq)]
    
   (interop/method Numbers "isZero" Double)
   [(il/ldc-r8 0.0)
    (il/ceq)]
   
   (interop/method Numbers "and" Int64 Int64)
   (il/and)
    
    ;;;
   
   (interop/method Numbers "unchecked_minus" Double)
   (il/neg)
   
   (interop/method Numbers "unchecked_minus" Int64)
   (il/neg)
  
   (interop/method Numbers "unchecked_minus" Int64 Double)
   (pop-convert Double (il/sub))
   
   (interop/method Numbers "unchecked_minus" Double Int64)
   [(il/conv-r8) (il/sub)]
   
   (interop/method Numbers "unchecked_minus" Int64 Int64)
   (il/sub)
   
   (interop/method Numbers "unchecked_minus" Double Double)
   (il/sub)
   
   (interop/method Numbers "divide" Double Int64)
   [(il/conv-r8) (il/div)]
   
   (interop/method Numbers "divide" Int64 Double)
   (pop-convert Double (il/div))
   
   (interop/method RT "uncheckedIntCast" Double)
   (il/conv-i4)
   
   (interop/method RT "uncheckedIntCast" Int64)
   (il/conv-i4)
   
   (interop/method RT "uncheckedFloatCast" Int64)
   (il/conv-r4)
   
   (interop/method RT "uncheckedFloatCast" Double)
   (il/conv-r4)
   
   (interop/method RT "uncheckedFloatCast" Single)
   nil
   
   (interop/method RT "uncheckedIntCast" Single)
   (il/conv-i4)
   
   (interop/method RT "floatCast" Int64)
   (il/conv-r4)
   
   (interop/method RT "longCast" Int64)
   []
   
   (interop/method RT "longCast" Int32)
   []
   
   (interop/method RT "uncheckedIntCast" Int32)
   []
   
   (interop/method RT "intCast" Int32)
   []
   
   (interop/method RT "intCast" Int64)
   [(il/conv-ovf-i4)]
   
   (interop/method RT "alength" Array)
   [(il/ldlen)
    (il/conv-i4)]
   
   (interop/method RT "aget" Array Int32)
   [(il/ldelem)
    (il/conv-i4)]
   
   (interop/method Numbers "unchecked_add" Double Int64)
   [(il/conv-r8)
    (il/add)]
      
   (interop/method clojure.lang.Util "equiv" Int64 Int64)
   (il/ceq)
   
   (interop/method clojure.lang.Util "equiv" Double Double)
   (il/ceq)
   
   (interop/method clojure.lang.Util "equiv" Object Object)
   (il/ceq)
   
   (interop/method Numbers "inc" Int64)
   [(il/ldc-i8 1)
    (il/add-ovf)]
   
   (interop/method Numbers "unchecked_inc" Int64)
   [(il/ldc-i8 1)
    (il/add)]
   
   (interop/method Numbers "add" Int64 Int64)
   [(il/add-ovf)
    (il/conv-i8)]
   
   (interop/method Numbers "add" Double Int64)
   [(il/conv-r8)
    (il/add)]
   
   (interop/method Numbers "add" Double Double)
   (il/add)
   
   (interop/method Numbers "unchecked_add" Double Double)
   (il/add)
   
   (interop/method Numbers "unchecked_dec" Int64)
   [(il/ldc-i4-1)
    (il/sub)]
   
   (interop/method Numbers "unchecked_add" Int64 Int64)
   (il/add)
   
   (interop/method Numbers "unchecked_add" Int64 Double)
   [(let [loc (il/local Double)]
      [(il/stloc loc)
       (il/conv-r8)
       (il/ldloc loc)
       (il/add)])]
   
   (interop/method Numbers "unchecked_multiply" Int64 Int64)
   (il/mul)
   
   (interop/method Numbers "multiply" Double Double)
   (il/mul)
   
   (interop/method Numbers "divide" Double Double)
   (il/div)
   
   (interop/method Numbers "unchecked_multiply" Double Double)
   (il/mul)
   
   (interop/method Numbers "unchecked_multiply" Double Int64)
   [(il/conv-r8)
    (il/mul)]
   
   (interop/method Numbers "unchecked_multiply" Int64 Double)
   [(let [loc (il/local Double)]
      [(il/stloc loc)
       (il/conv-r8)
       (il/ldloc loc)
       (il/mul)])]
   })

(defn intrinsics [symbolizers]
  (update symbolizers
          :static-method
          (fn [old-static-method-symbolizer]
            (fn intrinsic-static-method-symbolizer
              [{:keys [method args] :as ast} symbolizers]
              (if-let [intrinsic-bytecode (intrinsic-map method)]
                [(interleave
                   (map #(magic/symbolize % symbolizers) args)
                   (map magic/convert
                        (map clr-type args)
                        (interop/parameter-types method)))
                 intrinsic-bytecode]
                (old-static-method-symbolizer ast symbolizers))))))
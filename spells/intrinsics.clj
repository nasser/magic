(ns magic.spells.intrinsics
  (:require [magic.core :as magic]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [clojure.lang RT Numbers]))

(def intrinsic-map
  {(interop/method RT "uncheckedIntCast" Double)
   (il/conv-i4)
   
   (interop/method RT "uncheckedIntCast" Int64)
   (il/conv-i4)
   
   (interop/method RT "uncheckedFloatCast" Int64)
   (il/conv-r4)
   
   (interop/method RT "uncheckedIntCast" Single)
   (il/conv-i4)
   
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
   
   ;; TODO replace Numbers.add with ovf intrinsics when possible? ~40% faster
   (interop/method Numbers "lt" Int64 Int64)
   (il/clt)
   
   (interop/method Numbers "lt" Double Double)
   (il/clt)
      
   (interop/method Numbers "lt" Int64 Double)
   (il/clt)
   
   (interop/method Numbers "lte" Int64 Int64)
   (il/clt)
   
   (interop/method Numbers "gt" Int64 Int64)
   (il/cgt)
   
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
    (il/add-ovf)]
      
   (interop/method Numbers "add" Double Double)
   (il/add-ovf)
   
   (interop/method Numbers "unchecked_add" Double Double)
   (il/add)
   
   (interop/method Numbers "unchecked_dec" Int64)
   [(il/ldc-i4-1)
    (il/sub)]
    
   (interop/method Numbers "isPos" Int64)
   [(il/ldc-i4-0)
    (il/cgt)]
   
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
   
   (interop/method Numbers "multiply" Int32 Int32)
   (il/mul)
   
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
                [(map #(magic/symbolize % symbolizers) args)
                 intrinsic-bytecode]
                (old-static-method-symbolizer ast symbolizers))))))
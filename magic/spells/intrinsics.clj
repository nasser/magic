(ns magic.spells.intrinsics
  (:require [magic.core :as magic]
            [magic.analyzer.types :refer [clr-type numeric best-numeric-promotion]]
            [magic.interop :as interop]
            [mage.core :as il])
  (:import [clojure.lang RT Numbers]))

;; TODO are multimethods the best solution to this?
;; TODO add back symbolizers
(defmulti intrinsic-il (fn [v args] v))

(defmethod intrinsic-il :default [v args] nil)

(defmethod intrinsic-il nil [v args] nil)

(defmethod intrinsic-il #'+ [_ args]
  (case (count args)
    0 (il/ldc-i4-0)
    1 (when (numeric (clr-type (first args)))
        (magic/symbolize (first args)))
    (let [arg-types (map clr-type args)]
      (when (every? numeric arg-types)
        (let [promotion (best-numeric-promotion arg-types)]
          [(interleave
             (map magic/symbolize args)
             (map magic/convert arg-types (repeat promotion)))
           (repeat (dec (count args)) (il/add))])))))

;; TODO pass symbolizers in!
(defmethod intrinsic-il #'int [_ args]
  [(magic/symbolize (first args))
   (magic/convert (clr-type (first args))
                  Int32)])

(defn intrinsics [symbolizers]
  (-> symbolizers
      (update :invoke
              (fn [old-invoke-symbolizer]
                (fn intrinsic-invoke-symbolizer
                  [{:keys [fn args] :as ast} symbolizers]
                  (if-let [il (intrinsic-il (:var fn) args)]
                    il
                    (old-invoke-symbolizer ast symbolizers)))))
      (update :static-method
              (fn [old-static-method-symbolizer]
                (fn intrinsic-static-method-symbolizer
                  [{:keys [form args] :as ast} symbolizers]
                  (if-let [il (intrinsic-il (-> form meta :original-var) args)]
                    il
                    (old-static-method-symbolizer ast symbolizers)))))))
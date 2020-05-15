(ns magic.analyzer.binder
  (:require [magic.analyzer
             [util :refer [throw! var-interfaces] :as util]
             [reflection :refer [find-method]]])
  (:import [System.Reflection Binder BindingFlags MethodInfo MethodBase]))

;; TODO warn on truncate?
(defn convertible? [from to]
  (or (and (nil? from) (nil? to))
      (= from to)
      (and (nil? from) (not (.IsValueType to)))
      (= to Boolean)
      (and (= System.Void from) (not (.IsValueType to)))
      (find-method from "op_Implicit" to)
      (find-method from "op_Explicit" to)
      (and (.IsPrimitive from) (.IsPrimitive to))
      (and (.IsValueType from) (= to Object))
      (and (= from Object) (.IsValueType to))
      (.IsSubclassOf to from)
      (.IsSubclassOf from to)))

;; closeness
;; a rough implementaion of Eric Lippert's "closeness" concept 
;; https://ericlippert.com/2013/12/23/closer-is-better/
(defn closer-type?
  "A type is closer than another if it is a subtype or more primitive"
  [type-a type-b]
  (or (.IsSubclassOf type-a type-b)
      (and (.IsPrimitive type-a)
           (not (.IsPrimitive type-b)))))

(defn closer-score
  "Method A recieves one point for every parameter it has that is closer than method B's
  corresponding parameter"
  [meth-a meth-b]
  (reduce + (map #(if (closer-type? (.ParameterType %1)
                                    (.ParameterType %2))
                    1 0)
                 (.GetParameters meth-a)
                 (.GetParameters meth-b))))

(defn closer-sort
  "Closeness comparator. Closer methods are earlier."
  [a b]
  (let [a-b (closer-score a b)
        b-a (closer-score b a)]
    (cond (< a-b b-a)  1
          (> a-b b-a) -1
          (= a-b b-a)  0)))

(defn signature-match?
  "Does method have the right number of convertible arguments?"
  [method arg-types]
  (let [param-types (->> method .GetParameters (map #(.ParameterType %)))]
    (and (= (count param-types)
            (count arg-types))
         (every? true?
                 (map
                   #(convertible? %1 %2)
                   param-types
                   arg-types)))))

(def binder Magic.Binder/Shared)

(defn select-method
  ([methods arg-types]
   (select-method methods arg-types BindingFlags/Default nil))
  ([methods arg-types flags modifiers]
   (try
     (.SelectMethod
      binder
      flags
      (into-array MethodBase methods)
      (into-array Type arg-types)
      modifiers)
     (catch System.Reflection.AmbiguousMatchException e
       ;; TODO is it ok if this is silent?
       nil))))

(comment
  ;; https://stackoverflow.com/questions/14315437/get-best-matching-overload-from-set-of-overloads
  ;; https://msdn.microsoft.com/en-us/library/aa691339(v=vs.71).aspx
  (defn better-conversion [s t1 t2]
    (when (and (convertible? s t1)
               (convertible? s t2))
      (cond
        (= t1 t2) nil
        (= s t1) t1
        (= s t2) t2
        (and (convertible? t1 t2) (not (convertible? t2 t1)))
        t1
        (and (convertible? t2 t1) (not (convertible? t1 t2)))
        t2
        (and (= t1 SByte) (#{Byte UInt16 UInt32 UInt64} t2))
        t1
        (and (= t2 SByte) (#{Byte UInt16 UInt32 UInt64} t1))
        t2
        (and (= t1 Int16) (#{UInt16 UInt32 UInt64} t2))
        t1
        (and (= t2 Int16) (#{UInt16 UInt32 UInt64} t1))
        t2
        (and (= t1 Int32) (#{UInt32 UInt64} t2))
        t1
        (and (= t2 Int32) (#{UInt32 UInt64} t1))
        t2
        (and (= t1 Int64) (= UInt64 t2))
        t1
        (and (= t2 Int64) (= UInt64 t1))
        t2)))
  
  ;; https://msdn.microsoft.com/en-us/library/aa691338(v=vs.71).aspx
  #_
  (defn better-function-member
    [a p q]
    (let [a-types (map ast-type args)
          p-types (->> p .GetParameters (map #(.ParameterType %)))
          q-tyqes (->> p .GetParameters (map #(.ParameterType %)))]
      ))
  
  (defn specificity [sig]
    (->> (.GetParameters sig)
         (map #(-> (.ParameterType %) superchain count))
         (apply +)))
  
  ;; TODO sort by distance between sig and params, not specificity 
  (defn matching-signatures [sigs params]
    (->> sigs
         (filter (fn [sig]
                   (let [sig-params (map #(.ParameterType %) (.GetParameters sig))]
                     (and (= (count params)
                             (count sig-params))
                          (every? true? (map convertible? params sig-params))))))
         (sort-by specificity)
         reverse))
  
  (defn matching-methods [type name params]
    (let [sigs (filter #(= name (.Name %)) (.GetMethods type))]
      (matching-signatures sigs params)))
  
  (defn matching-constructors [type params]
    (matching-signatures (.GetConstructors type) params))
  
  (defn method-match?
    "Does are args convertible to method's parameters?"
    [method args]
    (let [params (.GetParameters method)]
      (and (= (count args)
              (count params))
           (->> (map
                  #(convertible? %1 %2)
                  (map #(.ParameterType %) params)
                  args)
                (remove identity)
                empty?)))))
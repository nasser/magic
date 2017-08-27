(ns magic.interfaces
  (:require [mage.core :as il])
  (:import [System.Reflection
            TypeAttributes
            MethodAttributes
            FieldAttributes
            FieldInfo
            MethodInfo
            PropertyInfo]))

(defn generic-params [n]
  (->> (range \A (+ \A n))
       (map (comp symbol str char))))

(defn generic-invoke [n]
  (let [params (generic-params n)
        ret (first params)
        args (vec (rest params))]
    (il/method
      "invoke"
      (enum-or MethodAttributes/Public
               MethodAttributes/Abstract
               MethodAttributes/Virtual)
      ret args [])))

(defn generic-function [name n]
  (let [params (generic-params n)
        ret (first params)
        args (vec (rest params))]
    (il/type
      (str name "`" n)
      (enum-or TypeAttributes/Interface
               TypeAttributes/Abstract
               TypeAttributes/Public)
      []
      nil
      params
      [(generic-invoke n)])))

(defn function-interfaces [n]
  (il/assembly+module
    "Interfaces"
    (map 
      #(generic-function "Function" %)
      (range 1 n))))

(defn emit! []
  (il/emit!
    (function-interfaces 20)))
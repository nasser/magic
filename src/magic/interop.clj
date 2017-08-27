(ns magic.interop
  (:require [magic.analyzer.util :refer [throw!]]))

(defn method
  ([t name & params]
   (try
     (.GetMethod t name (into-array Type params))
     (catch Exception e
       (throw (Exception. (str "Broken! "
                               (type e) ", "
                               t ", "
                               (type t) ", "
                               name ", "
                               (type name) ", "
                               params ", "
                               (type params) ", "
                               )))))))

(defn parameters
  [method] (.GetParameters method))

(defn parameter-types
  [method] (map #(.ParameterType %)
                (parameters method)))

(defn field
  ([type name] (.GetField type name)))

(defn constructor
  ([type & params] (.GetConstructor type (into-array Type params))))

(defn getter [type name]
  (method type (str "get_" name)))

(defn setter [type name]
  (method type (str "set_" name)))

(defn generic-type [name params]
  (let [base-type (clojure.lang.RT/classForName (str name "`" (count params)))]
    (.MakeGenericType base-type (into-array Type params))))
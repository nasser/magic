(ns magic.interop
  (:require [magic.analyzer.util :refer [throw!]])
  (:require [clojure.string :as string]))

(defn- parameters-match [params]
  (fn [method]
    (let [parameter-types (map #(.ParameterType %) (.GetParameters method))]
      (and (= (count parameter-types)
              (count params))
           (every? #(= (first %) (last %))
                   (map vector parameter-types params))))))

(def method
  (memoize
   (fn [t name & params]
     (try
       (let [methods (.GetMethods t)
             name-matches (filter #(= name (.Name %)) methods)
             parameter-matches
             (filter (parameters-match params) name-matches)]
         (case (count parameter-matches)
           1 (first parameter-matches)
           0 nil
           (throw (System.Reflection.AmbiguousMatchException.
                   (str t "::" name " " (string/join "," params) " " (vec parameter-matches))))))))))

(defn parameters
  [method] (.GetParameters method))

(defn parameter-types
  [method] (map #(.ParameterType %)
                (parameters method)))

(def field
  (memoize
   (fn [type name] (.GetField type name))))

(def constructor
  (memoize
   (fn [type & params] (.GetConstructor type (into-array Type params)))))

(def getter
  (memoize
   (fn [type name] (method type (str "get_" name)))))

(def setter
  (memoize
   (fn [type name] (method type (str "set_" name)))))

(defn generic-type [name params]
  (when-let [base-type (clojure.lang.RT/classForName (str name "`" (count params)))]
    (.MakeGenericType base-type (into-array Type params))))
(ns magic.analyzer.reflection)

(defn find-method
  ([type name & params] (.GetMethod type name (into-array Type params))))

(defn find-field
  ([type name] (.GetField type name)))

(defn find-constructor
  ([type & params] (.GetConstructor type (into-array Type params))))
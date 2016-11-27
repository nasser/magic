(ns magic.interop)

(defn method
  ([type name & params] (.GetMethod type name (into-array Type params))))

(defn field
  ([type name] (.GetField type name)))

(defn constructor
  ([type & params] (.GetConstructor type (into-array Type params))))

(defn getter [type name]
  (method type (str "get_" name)))

(defn setter [type name]
  (method type (str "set_" name)))
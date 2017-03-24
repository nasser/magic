(ns magic.analyzer.types
  (:refer-clojure :exclude [resolve])
  (:require [magic.analyzer
             [util :refer [throw! var-interfaces]]
             [reflection :refer [find-method]]]))

(defn read-generic-name [name]
  (let [reader (-> name str
                 System.IO.StringReader.
                 clojure.lang.PushbackTextReader.)]
    [(read reader) (read reader)]))

(defn class-for-name [s]
  (if s
    (clojure.lang.RT/classForName (str s))))

;; TODO look into this, does this do anything useful?  
(defn maybe-class [c]
  (or (class-for-name c) c))

(defn superchain [t]
  (if-let [b (.BaseType t)]
    (cons b (superchain b))))

(defn zero-arity-type [class name]
  (or (if-let [info (.GetField class name)]
        (.FieldType info))
      (if-let [info (.GetProperty class name)]
        (.PropertyType info))
      (if-let [info (.GetMethod class name Type/EmptyTypes)]
        (.ReturnType info))))

(defn n-arity-type [class method params]
  (if-let [info (.GetMethod class name params)]
    (.ReturnType info)))

(defn zero-arity-info [class name]
  (or (.GetField class name)
      (.GetProperty class name)
      (.GetMethod class name Type/EmptyTypes)))

(defn resolve
  ([t]
   (if (symbol? t)
     (or (clojure.core/resolve t)
         (throw! "Could not resolve " t " as  type."))
     t))
  ([t ast]
   (if (symbol? t)
     (or (clojure.core/resolve t)
         (throw! "Could not resolve " t " as  type in " (:form ast)))
     t)))

(defn tag [x]
  (if-let [t (-> x meta :tag)]
    (resolve t)))

;; TODO warn on truncate?
(defn convertable? [from to]
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
                        (every? true? (map convertable? params sig-params))))))
       (sort-by specificity)
       reverse))

(defn matching-methods [type name params]
  (let [sigs (filter #(= name (.Name %)) (.GetMethods type))]
    (matching-signatures sigs params)))

(defn matching-constructors [type params]
  (matching-signatures (.GetConstructors type) params))

(defn method-match?
  "Does are args convertable to method's parameters?"
  [method args]
  (let [params (.GetParameters method)]
    (and (= (count args)
            (count params))
         (->> (map
                #(convertable? %1 %2)
                (map #(.ParameterType %) params)
                args)
              (remove identity)
              empty?))))

(defmulti clr-type
  "The CLR type of an AST node"
  :op)

(defn non-void-clr-type
  ([ast]
   (non-void-clr-type ast Object))
  ([ast non-void-type]
   (let [t (clr-type ast)]
     (if (= t System.Void)
       non-void-type
       t))))

(defn best-match
  ;; TODO implement better overload resolution
  ;; e.g. https://ericlippert.com/2013/12/23/closer-is-better/
  "Given a sequence of argument ASTs and a sequence of MethodInfos returns
   the best match"
  [args methods]
  (let [arg-types (map clr-type args)]
    (->> methods
         (filter #(method-match? % arg-types))
         first)))

(defmethod clr-type :default [ast]
  (throw! "clr-type not implemented for :op " (:op ast) " in AST " (pr-str ast)))

;; TODO remove
(defmethod clr-type nil
  [ast]
  Object
  #_
  (throw! "clr-type not implemented for nil"))

(defmethod clr-type :do
  [{:keys [ret] :as ast}] (clr-type ret))

(defmethod clr-type :set!
  [{:keys [val] :as ast}] (clr-type val))

(defmethod clr-type :quote
  [{:keys [expr] :as ast}] (clr-type expr))

(defmethod clr-type :maybe-class
  [{:keys [class] :as ast}]
  (resolve class ast))

(defmethod clr-type :var [ast]
  Object)

(defmethod clr-type :the-var [ast]
  Object)

(defmethod clr-type :const [ast]
  (if (= :class (:type ast))
    System.Type ;; (:val ast) ; oh jesus
    (or (type (:val ast)) Object))) ;; nil -> Object

(defmethod clr-type :vector [ast]
  clojure.lang.IPersistentVector)

(defmethod clr-type :set [ast]
  clojure.lang.IPersistentSet)

(defmethod clr-type :map [ast]
  clojure.lang.IPersistentMap)

(defmethod clr-type :static-method [ast]
  (if (ast :inexact?)
    (.ReturnType (best-match ast :methods))
    (.ReturnType (ast :method))))

(defmethod clr-type :instance-method [ast]
  (if (ast :inexact?)
    (.ReturnType (best-match ast :methods))
    (.ReturnType (ast :method))))

(defmethod clr-type :static-property [ast]
  (-> ast :property .PropertyType))

(defmethod clr-type :instance-property [ast]
  (-> ast :property .PropertyType))

(defmethod clr-type :static-field [ast]
  (-> ast :field .FieldType))

(defmethod clr-type :instance-field [ast]
  (-> ast :field .FieldType))

;; TODO dynamics always typed as object?
(defmethod clr-type :dynamic-field [ast]
  System.Object)

(defmethod clr-type :invoke
  [{:keys [fn args]}]
  (resolve (or (->> fn
                    :meta
                    :arglists
                    (filter #(= (count %) (count args)))
                    first
                    meta
                    :tag)
               (let [arg-types (map clr-type args)
                     target-interfaces (var-interfaces fn)
                     exact-match (->> target-interfaces
                                      (filter #(= (drop 1 (.GetGenericArguments %))
                                                  arg-types))
                                      first)]
                 (if exact-match
                   (first (.GetGenericArguments exact-match))))
               'Object)))

(defmethod clr-type :new [ast]
  (:type ast))

(defmethod clr-type :initobj [ast]
  (:type ast))

(defmethod clr-type :maybe-host-form
  [ast]
  (throw! "Trying to find type of :maybe-host-form in " ast))

(defmethod clr-type :host-interop
  [ast]
  (throw! "Trying to find type of :host-interop in " ast))

;; TODO -> form locals :form meta :tag ??
(defmethod clr-type :binding [ast]
  (or
    (if-let [tag (-> ast :form meta :tag)]
      (if (symbol? tag)
        (resolve tag)
        tag))
    (if-let [init (:init ast)]
      (clr-type init))
    Object))

(defmethod clr-type :local
  [{:keys [name init form local by-ref?] {:keys [locals]} :env}]
  (let [tag (-> form locals :form meta :tag)
        type (cond tag
                   (if (symbol? tag)
                     (resolve tag)
                     tag)
                   (= local :arg)
                   Object
                   :else
                   (clr-type init))]
    (if by-ref?
      (.MakeByRefType type)
      type)))

(defmethod clr-type :let [ast]
  (-> ast :body :ret clr-type))

(defmethod clr-type :loop [ast]
  (-> ast :body :ret clr-type))

;; TODO ????
(defmethod clr-type :recur [ast]
  System.Void)

(defn always-then?
  "Is an if AST node always true?"
  [{:keys [op test then else]}]
  (and (= op :if)
       (or (and (:literal? test)
                (not= (:val test) false)
                (not= (:val test) nil))
           (and (.IsValueType (clr-type test))
                (not= Boolean (clr-type test))))))

(defn always-else?
  "Is an if AST node always false?"
  [{:keys [op test then else]}]
  (and (= op :if)
       (:literal? test)
       (or (= (:val test) false)
           (= (:val test) nil))))

(defmethod clr-type :if
  [{:keys [form test then else] :as ast}]
  (if-let [t (tag form)]
    t
    (let [then-type (clr-type then)
          else-type (clr-type else)]
      (cond
        (= then-type else-type) then-type
        (always-then? ast) then-type
        (always-else? ast) else-type
        ;; TODO do these make sense? are they just for recur?
        (= then-type System.Void) else-type  
        (= else-type System.Void) then-type
        ;; TODO compute common type  
        :else Object))))
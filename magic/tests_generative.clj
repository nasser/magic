(ns magic.tests-generative
  (:refer-clojure :exclude [any? methods])
  (:require
    [clojure.string :as string]
    [clojure.test.check.properties :refer [for-all for-all*]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check :as check]
    [magic.analyzer :as clr]
    [magic.analyzer.types :refer [clr-type]]
    [magic.core :as magic])
  (:import [System.Reflection Assembly]))

(in-ns 'clojure.test.check.generators)

(defn s-expression*
  [& generators]
  (gen-bind (sequence gen-bind gen-pure generators)
            (fn [roses]
              (gen-pure (rose/zip core/list* roses)))))

(defn s-expression
  [& generators]
  (gen-bind (sequence gen-bind gen-pure generators)
            (fn [roses]
              (gen-pure (rose/zip core/list roses)))))

(in-ns 'magic.tests-generative)

(def mscorlib (Assembly/Load "mscorlib"))

(assembly-load-from "/Applications/Unity/Unity.app/Contents/Managed/UnityEngine.dll")
(assembly-load-from "/Users/nasser/Scratch/hummus/OpenTK.dll")

(def single
  (gen/sized (fn [size] (gen/return (- (rand size) (/ size 2))))))

(defn any? [pred coll]
  (cond
    (empty? coll) false
    (pred (first coll)) true
    :else (boolean (any? pred (next coll)))))

(def ^:dynamic *testing-types*
  []
  #_
  (filter #(.IsVisible %) (.GetTypes mscorlib)))

(defn has-problematic-params? [method]
  (->> method
       .GetParameters
       (any? #(or (-> % .ParameterType .IsByRef)
                  (-> % .ParameterType .IsPointer)))))

(defn constructors
  ([type] (constructors type *testing-types*))
  ([type types]
    (if (.IsValueType type)
      ;; hack for initobj that does not show up in reflection
      (conj (seq (.GetConstructors type)) type)
      (->> (.GetConstructors type)
           (remove has-problematic-params?)))))

(defn fields
  ([type] (fields type *testing-types*))
  ([type types]
    (->> types (mapcat #(.GetFields %)) (filter #(= type (.FieldType %))))))

(defn properties
  ([type] (properties type *testing-types*))
  ([type types]
    (->> types (mapcat #(.GetProperties %)) (remove #(= "Item" (.Name %))) (filter #(= type (.PropertyType %))))))

;; TODO byrefs
(defn methods
  ([type] (methods type *testing-types*))
  ([type types]
    (->> types
      (mapcat #(.GetMethods %))
      (remove has-problematic-params?)
      (filter #(= type (.ReturnType %))))))

;; TODO maybe dont need?
(defn elements [seq]
  (when-not (empty? seq) (gen/elements seq)))

(defn gen-interop [type]
  (let [opts (remove nil?
              [(-> type constructors elements)
               (-> type fields elements)
               (-> type properties elements)
               (-> type methods elements)])]
  (if (empty? opts)
    (throw (Exception. (str "Cursed Type! " type)))
    (gen/one-of opts)))
  #_
  (->>
    [(-> type constructors elements)
     (-> type fields elements)
     (-> type properties elements)
     (-> type methods elements)]
     (remove nil?)
     gen/one-of))

(defmulti interop-form type)
(declare expression)

(defmethod interop-form
  System.Reflection.FieldInfo [field]
  (if (.IsStatic field)
    (gen/s-expression
      (-> '. gen/return)
      (-> field .DeclaringType .FullName symbol gen/return)
      (-> field .Name symbol gen/return))
    (gen/s-expression
      (-> '. gen/return)
      (-> field .DeclaringType expression)
      (-> field .Name symbol gen/return))))

(defmethod interop-form
  System.Reflection.PropertyInfo [property]
  (if (-> property .GetGetMethod .IsStatic)
    (gen/s-expression
      (-> '. gen/return)
      (-> property .DeclaringType .FullName symbol gen/return)
      (-> property .Name symbol gen/return))
    (gen/s-expression
      (-> '. gen/return)
      (-> property .DeclaringType expression)
      (-> property .Name symbol gen/return))))

(defmethod interop-form
  System.Reflection.MethodInfo [method]
  (if (.IsStatic method)
    (gen/s-expression*
      (-> '. gen/return)
      (-> method .DeclaringType .FullName symbol gen/return)
      (-> method .Name symbol gen/return)
      (->> method .GetParameters (map #(expression (.ParameterType %))) (apply gen/tuple)))
    (gen/s-expression*
      (-> '. gen/return)
      (-> method .DeclaringType expression)
      (-> method .Name symbol gen/return)
      (->> method .GetParameters (map #(expression (.ParameterType %))) (apply gen/tuple)))))

(defmethod interop-form
  System.Reflection.ConstructorInfo [ctor]
  (gen/s-expression*
    (-> 'new gen/return)
    (-> ctor .DeclaringType .FullName symbol gen/return)
    (->> ctor .GetParameters (map #(expression (.ParameterType %))) (apply gen/tuple))))

;; initobj hack!!
(defmethod interop-form
  Type [t]
  (gen/s-expression
    (-> 'new gen/return)
    (-> t .FullName symbol gen/return)))

;; maybe dont need, move to defmethod expression Object?
;; rename interop-form gen-interop-form?
(defn gen-interop-form [type]
  (gen/bind (gen-interop type)
    interop-form))

(defmulti expression identity)

(defmethod expression Object [t]
  (gen-interop-form t))

(defmethod expression Int16 [t]
  (gen/frequency
    [[2 (gen/choose Int16/MinValue Int16/MaxValue)]
     [1 (gen-interop-form t)]]))

(defmethod expression Int32 [t]
  (gen/frequency
    [[2 (gen/choose Int32/MinValue Int32/MaxValue)]
     [1 (gen-interop-form t)]]))

(defmethod expression Int64 [t]
  (gen/frequency
    [[2 (gen/choose Int64/MinValue Int64/MaxValue)]
     [1 (gen-interop-form t)]]))

(defmethod expression Single [t]
  (gen/frequency
    [[2 single]
     [1 (gen-interop-form t)]]))

(defmethod expression Double [t]
  (gen/frequency
    [[2 single]
     [1 (gen-interop-form t)]]))

(defmethod expression Byte [t]
  (gen/frequency
    [[2 gen/byte]
     [1 (gen-interop-form t)]]))

(defmethod expression Boolean [t]
  (gen/frequency
    [[2 gen/boolean]
     [1 (gen-interop-form t)]]))

(defmethod expression String [t]
  (gen/frequency
    [[2 gen/string]
     [1 (gen-interop-form t)]]))


(defn test-ctors []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [ctor-invoke-form (-> OpenTK.Vector3 constructors elements (gen/bind interop-form))]
        (let [ast (clr/analyze ctor-invoke-form)]
          (and (or (= (:op ast) :new)
                   (= (:op ast) :initobj))
               (= OpenTK.Vector3 (clr-type ast))))))))

;; 
(defn test-fields []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [field-get-form (-> Single fields elements (gen/bind interop-form))]
        (let [ast (clr/analyze field-get-form)]
          (and (or (= (:op ast) :static-field)
                   (= (:op ast) :instance-field))
               (= Single (clr-type ast))))))))

(defn test-properties []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [property-get-form (-> Single properties elements (gen/bind interop-form))]
        (let [ast (clr/analyze property-get-form)]
          (and (or (= (:op ast) :static-property)
                   (= (:op ast) :instance-property))
               (= Single (clr-type ast))))))))

(defn test-methods []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      5
      (for-all
        [method-invoke-form (-> Single methods elements (gen/bind interop-form))]
        (let [ast (clr/analyze method-invoke-form)]
          (and (or (= (:op ast) :static-method)
                   (= (:op ast) :instance-method))
               (= Single (clr-type ast))))))))


(comment   
  (defn run-tests [n type interop-fn validation-fn]
    (check/quick-check
      n
      (for-all*
        [(-> type interop-fn gen/elements (gen/bind interop-form))]
        (fn [form] (let [ast (clr/analyze form)]
                     (and (= type (clr-type ast))
                          (validation-fn ast)))))))
  
  (binding [*testing-types* [UnityEngine.GameObject UnityEngine.Vector3 UnityEngine.Vector2]]
    (run-tests 50
               UnityEngine.Vector3
               methods
               #(or (= (:op %) :instance-method)
                    (= (:op %) :static-method))))
  
  (binding [*testing-types* [UnityEngine.GameObject UnityEngine.Vector3 UnityEngine.Vector2 UnityEngine.Vector4]]
    (check/quick-check
      5
      (for-all*
        [(-> UnityEngine.GameObject methods elements (gen/bind interop-form))]
        (fn [form] (let [ast (clr/analyze form)]
                     (= UnityEngine.GameObject (clr-type ast))))))
    )
  
  
  (def generated-fn
    (magic/compile-fn
      (list* 'fn []
             (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix4]]
               (-> Single methods elements (gen/bind interop-form) (gen/sample 1))))))
  
  (binding [*testing-types* [UnityEngine.Vector3 UnityEngine.Vector2 UnityEngine.GameObject UnityEngine.Transform]]
    (check/quick-check
      5
      (for-all*
        [(-> UnityEngine.Vector3 methods gen/elements (gen/bind interop-form))]
        (fn [form]
          (let [fn-form (list 'fn [] form)
                magic-fn (magic/compile-fn fn-form)
                clojure-fn (eval fn-form)
                clojure-result (volatile! nil)
                ]
            (try
              (vreset! clojure-result (clojure-fn))
              (catch Exception e
                (vreset! clojure-result e)))
            (try 
              (= (magic-fn) @clojure-result)
              (catch Exception e
                (= (type e) (type @clojure-result))))))))))

;; [x] by-ref arguments (Vector3.SmoothDamp)
;; [x] Cursed Type! System.SByte*
;; [ ] interfaces?
;; [ ] System.Globalization.Calendar
;; [ ] Item property
;; [x] analyze failing on castable parameters
;; [x] increase chance of picking literal
;; [ ] control chance of picking literal
;; [x] *testing-types* not honored deeper in chain?
;; [ ] op_Implicit can be ambiguous (Vector4.op_Implicit)
;; 
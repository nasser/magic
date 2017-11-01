(ns magic.tests-generative
  (:refer-clojure :exclude [any? methods])
  (:require
    [clojure.string :as string]
    [clojure.pprint :as pp]
    [clojure.test.check.properties :refer [for-all for-all*]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check :as check]
    magic.intrinsics
    [mage.core :as il]
    [magic.analyzer :as clr]
    [magic.analyzer.types :refer [ast-type]]
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
    (gen/one-of opts))))

(defmulti interop-form type)
;; TODO rename expression*
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

;; types for which an expression multimethod path exists
;; primitives + object & string
(def basic-types
  [Object Int16 Int32 Int64 Single Double Byte Boolean String])

(defmulti expression identity)

(defmethod expression Object [t]
  (gen-interop-form t))

;; TODO 0 - max avoids dumb negative number bugs, but does it miss things?
(defmethod expression Int16 [t]
  (gen/frequency
    [[20 (gen/choose 0 Int16/MaxValue)]
     [15 (gen-interop-form t)]]))

(defmethod expression Int32 [t]
  (gen/frequency
    [[20 (gen/choose 0 Int32/MaxValue)]
     [15 (gen-interop-form t)]]))

(defmethod expression Int64 [t]
  (gen/frequency
    [[20 (gen/choose 0 Int64/MaxValue)]
     [15 (gen-interop-form t)]]))

(defmethod expression Single [t]
  (gen/frequency
    [[20 single]
     [15 (gen-interop-form t)]]))

(defmethod expression Double [t]
  (gen/frequency
    [[20 single]
     [15 (gen-interop-form t)]]))

(defmethod expression Byte [t]
  (gen/frequency
    [[20 gen/byte]
     [15 (gen-interop-form t)]]))

(defmethod expression Boolean [t]
  (gen/frequency
    [[20 gen/boolean]
     [15 (gen-interop-form t)]]))

(defmethod expression String [t]
  (gen/frequency
    [[20 gen/string]
     [15 (gen-interop-form t)]]))

(def s-expressions
  (gen/elements [:if :< :>]))

(defmulti s-expression identity)

(declare expr)

(defmethod s-expression
  :if [_]
  (gen/s-expression
    (gen/return 'if)
    (gen/frequency
      [[3 (expression Boolean)]
       [1 (expr)]])
    (expr)
    (expr)))

(defmethod s-expression
  :< [_]
  (gen/s-expression
    (gen/return '<)
    (gen/frequency
      [[1 (expression Int64)]
       [1 (expression Int32)]
       [1 (expression Int16)]
       [1 (expression Byte)]
       [1 (expression Single)]
       [1 (expression Double)]
       [1 (expr)]])
    (gen/frequency
      [[1 (expression Int64)]
       [1 (expression Int32)]
       [1 (expression Int16)]
       [1 (expression Byte)]
       [1 (expression Single)]
       [1 (expression Double)]
       [1 (expr)]])))

(defmethod s-expression
  :> [_]
  (gen/s-expression
    (gen/return '>)
    (gen/frequency
      [[1 (expression Int64)]
       [1 (expression Int32)]
       [1 (expression Int16)]
       [1 (expression Byte)]
       [1 (expression Single)]
       [1 (expression Double)]
       [1 (expr)]])
    (gen/frequency
      [[1 (expression Int64)]
       [1 (expression Int32)]
       [1 (expression Int16)]
       [1 (expression Byte)]
       [1 (expression Single)]
       [1 (expression Double)]
       [1 (expr)]])))

;; TODO rename expression
(defn expr []
  (gen/frequency
    [[2 (gen/one-of
          (->> *testing-types*
               (concat basic-types)
               (map expression)))]
     [1 (gen/bind
          s-expressions
          s-expression)]]))

;; TODO new verify namespace
(defn- load-path [assemblies]
  (->> assemblies
       (map assembly-load-with-partial-name)
       (map #(.DirectoryName (System.IO.FileInfo. (.Location %))))
       (string/join System.IO.Path/PathSeparator)))

;; TODO dont hard code load path
(defn verify [assembly]
  (let [psi (System.Diagnostics.ProcessStartInfo.)
        proc (System.Diagnostics.Process.)
        lp (load-path ["Interfaces" "Constants" "Clojure" "OpenTK"])
        sb (StringBuilder.)]
    (set! (.FileName psi) "peverify")
    (set! (.Arguments psi) assembly)
    (set! (.UseShellExecute psi) false)
    (set! (.RedirectStandardOutput psi) true)
    (set! (.CreateNoWindow psi) true)
    (.. psi EnvironmentVariables (Add "MONO_PATH" lp))
    (set! (.StartInfo proc) psi)
    (.Start proc)
    (while (not (.. proc StandardOutput EndOfStream))
      (.AppendLine sb (.. proc StandardOutput ReadLine)))
    (str sb)))

(defn verify? [assembly]
  (empty? (verify assembly)))

(defn verify! [assembly]
  (let [res (verify assembly)]
    (when-not (empty? res)
      (throw (ex-info "Verification Failure" {:message res})))))

(defn test-ctors []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [ctor-invoke-form (-> OpenTK.Vector3 constructors elements (gen/bind interop-form))]
        (let [ast (clr/analyze ctor-invoke-form)]
          (and (or (= (:op ast) :new)
                   (= (:op ast) :initobj))
               (= OpenTK.Vector3 (ast-type ast))))))))

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
               (= Single (ast-type ast))))))))

(defn test-properties []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [property-get-form (-> Single properties elements (gen/bind interop-form))]
        (let [ast (clr/analyze property-get-form)]
          (and (or (= (:op ast) :static-property)
                   (= (:op ast) :instance-property))
               (= Single (ast-type ast))))))))

(defn test-methods []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      5
      (for-all
        [method-invoke-form (-> Single methods elements (gen/bind interop-form))]
        (let [ast (clr/analyze method-invoke-form)]
          (and (or (= (:op ast) :static-method)
                   (= (:op ast) :instance-method))
               (= Single (ast-type ast))))))))

(defn expressions-analyze []
  (binding [*testing-types* [OpenTK.Vector3 OpenTK.Vector2 OpenTK.Matrix3]]
    (check/quick-check
      50
      (for-all
        [expr-form (expr)]
        (clr/analyze expr-form)))))

(defn expressions-compile [n]
  (check/quick-check
    n
    (for-all
      [expr-form (expr)]
      (let [form-string (with-out-str (clojure.pprint/pprint expr-form))
            filename (str (gensym "magic.test"))
            w 9000]
        (Console/WriteLine)
        (Console/WriteLine "Testing ")
        (Console/WriteLine (if (> (count form-string) w)
                             (str (subs form-string 0 (- w 3)) "...")
                             form-string))
        (Console/Write "    Compiling...")
        (->> expr-form
             (list 'fn [])
             clr/analyze
             magic/compile
             (il/assembly+module filename)
             il/emit!)
        (Console/WriteLine "OK")
        (Console/Write "    Verifying...")
        (try
          (verify! (str filename ".dll"))
          (Console/WriteLine "OK")
          (catch Exception e
            (Console/WriteLine "FAIL")
            (throw e)))
        true))))

(comment   
  (use 'clojure.pprint)
  
  (nostrand.core/reference "/Users/nasser/Scratch/loadtimes/Compiled/UnityEngine.dll")
  
  ;; TODO better handling of e.g. verification ex-infos
  (time
    (binding [*unchecked-math* true
              *testing-types* [UnityEngine.Vector3
                               UnityEngine.Vector2
                               UnityEngine.Matrix4x4
                               UnityEngine.GameObject
                               UnityEngine.Transform
                               UnityEngine.BoxCollider
                               UnityEngine.Camera
                               UnityEngine.Mesh
                               UnityEngine.Rigidbody]]
      (let [res (expressions-compile 100)]
        (if (= true (:result res))
          (println "Passed" (:num-tests res) "tests" "\nseed:" (:seed res))
          (let [smallest-fail (-> res :shrunk :smallest first)
                smallest-message (-> res :shrunk :result ex-data :message)]
            (println smallest-message)
            (pp/pprint smallest-fail))))))
  
  (pprint
    (->> (gen/sample if-expr 10)
         ; (map count)
         ; (into #{})
         ))
  
  (ex-info "A" {})
  
  (gen/sample
    (gen/one-of
      (->> *testing-types*
           (concat basic-types)
           (map expression))))
  
  
  (binding [*testing-types* [UnityEngine.Vector3]]
    (pprint (gen/sample (expr) 10)))
  
  (assembly-load-from "/Applications/Unity/Unity.app/Contents/Managed/UnityEngine.dll")
  (assembly-load-from "OpenTK.dll")
  
  (defn run-tests [n type interop-fn validation-fn]
    (check/quick-check
      n
      (for-all*
        [(-> type interop-fn gen/elements (gen/bind interop-form))]
        (fn [form] (let [ast (clr/analyze form)]
                     (and (= type (ast-type ast))
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
                     (= UnityEngine.GameObject (ast-type ast))))))
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
                (= (type e) (type @clojure-result)))))))))
  )

;;) [x] by-ref arguments (Vector3.SmoothDamp)
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
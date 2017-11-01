(ns magic.analyzer.tests
  (:require [magic.analyzer :as clr]
            [magic.analyzer.types :refer [ast-type]])
  (:use clojure.test))

(defn tests [& fns]
  (fn [frm frm*]
    (testing (str frm)
      (doseq [f fns]
        (f frm frm*)))))

(defn should-be [fns & frms]
  (let [testfn (apply tests fns)]
    (doseq [frm frms]
      (let [frm* (clr/analyze frm)]
        (testfn frm frm*)))))

;; TODO other exceptions
(defn should-throw [& frms]
  (doseq [frm frms]
    (testing (str frm)
      (is (thrown? System.Exception
                   (clr/analyze frm))))))

(defn op? [frm* op]
  (is (= (:op frm*)
         op)))

(defn literal [frm frm*]
  (is (:literal? frm*)))

(defn const [frm frm*]
  (is (= (:op frm*) :const)))

(defn correct-type [frm frm*]
  (is (= (-> frm* :val)
         (resolve frm))))

(defn test-type [frm]
  (testing (str frm)))

(deftest primitive-types
  (should-be
    [const correct-type literal]
    'String 'Int16 'Int64
    'Int32 'Single 'Double
    'Char 'Byte 'DateTime
    'Decimal 'Boolean 'UInt32
    'UInt64 'Object))

(deftest qualified-types
  (should-be
    [const correct-type literal]
    'System.String 'System.Int16 'System.Int64
    'System.Int32 'System.Single 'System.Double
    'System.Char 'System.Byte 'System.DateTime
    'System.Decimal 'System.Boolean 'System.UInt32
    'System.UInt64 'System.Object
    'System.Drawing.Point))

;; TODO depends on import analysis
#_
(deftest imported-type
  (test-type '(do (import System.IO.File) File)))

(deftest missing-types
  (should-throw
    'Flambix 'string 'SystemInt64
    'Int63 'System.Prambo))

;; host 
(defn inexact [kw]
  (let [info-type {:methods |System.Reflection.MethodInfo[]|
                   :constructors |System.Reflection.ConstructorInfo[]|}]
    (fn [frm frm*]
      (and (is (-> frm* :inexact?))
           (is (isa? (-> frm* kw into-array type)
                     (info-type kw)))
           (doseq [m (-> frm* kw)]
             (is (= (-> m .GetParameters count)
                    (-> frm* :args count))))
           ))))

(defn exact [kw]
  (let [info-type {:method System.Reflection.MethodInfo
                   :constructor System.Reflection.ConstructorInfo}]
    (fn [frm frm*]
      (and (is (not (-> frm* :inexact?)))
           (is (isa? (-> frm* kw type)
                     (info-type kw)))
           (is (= (-> frm* kw .GetParameters count)
                  (-> frm* :args count)))
           (is (= (->> frm* kw .GetParameters (map #(.ParameterType %)) vec)
                  (->> frm* :args (map ast-type) vec)))))))

(defn property [frm frm*]
  (is (isa? (-> frm* :property type)
            System.Reflection.PropertyInfo)))

(defn static-property [frm frm*]
  (and (op? frm* :static-property)
       (is (= (name frm)
              (-> frm* :property .Name)))))

(defn instance-property [frm frm*]
  (and (op? frm* :instance-property)
       (is (= (-> frm* :form last name)
              (-> frm* :property .Name)))))

(defn field [frm frm*]
  (is (isa? (-> frm* :field type)
            System.Reflection.FieldInfo)))

(defn static-field [frm frm*]
  (and (op? frm* :static-field)
       (is (= (name frm)
              (-> frm* :field .Name)))))

(defn instance-field [frm frm*]
  (and (op? frm* :instance-field)
       (is (= (-> frm* :form last name)
              (-> frm* :field .Name)))))

(deftest static-properties
  (should-be
    [property static-property]
    'DateTime/Now))

(deftest static-fields
  (should-be
    [static-field]
    'Int32/MaxValue 'Int64/MaxValue
    'Int32/MinValue 'Int64/MinValue
    'System.Int32/MaxValue 'System.Int64/MaxValue
    'System.Int32/MinValue 'System.Int64/MinValue)
  (should-throw
    'Int32/Max_Value
    'DateTime/Parse))

(defn static-method [frm frm*]
  (op? frm* :static-method))

(defn instance-method [frm frm*]
  (op? frm* :instance-method))

(deftest static-methods
  (should-be
    [static-method (exact :method)]
    '(DateTime/Compare DateTime/Now DateTime/Now)
    '(DateTime/Compare (DateTime.) (DateTime.))
    '(DateTime/FromBinary 89)
    '(DateTime/Parse "89"))
  
  (should-be
    [static-method (inexact :methods)]
    '(DateTime/Compare 1 2)
    '(DateTime/FromBinary "89")
    '(DateTime/Parse 1)
    '(DateTime/Parse 1 {:foo 12}))
  
  (should-throw
    '(DateTime/Jango 1 2)
    '(DateTime/Parse)
    '(DateTime/Parse :lots :of :arguments :way :too :many)))

(defn constructor [frm frm*]
  (op? frm* :new))

(defn initobj-constructor [frm frm*]
  (and (op? frm* :initobj)
       (is (= (-> frm* :form second name symbol resolve)
              (-> frm* :type)))))

(deftest value-type-constructors
  (should-throw
    '(DateTime. :lots :of :arguments :way :too))
  (should-be
    [constructor (exact :constructor)]
    '(DateTime. 1))
  (should-be
    [constructor (inexact :constructors)]
    '(System.Drawing.Point. 1)
    '(System.Drawing.Point. 1 2)
    '(System.Drawing.Size. 1 2)) 
  (should-be
    [initobj-constructor]
    '(DateTime.)
    '(System.Drawing.Point.)
    '(System.Drawing.Size.)))

(deftest constructors
  (should-be
    [constructor (exact :constructor)]
    '(System.Drawing.Bitmap. "Hello")
    '(System.Drawing.Bitmap. "Hello" false)
    '(System.Collections.Generic.List|[String]|.))
  (should-be
    [constructor (inexact :constructors)]
    '(System.Drawing.Bitmap. 8 8)
    '(System.Drawing.Bitmap. :foo :bar)
    '(System.Collections.Generic.List|[String]|. 45))
  (should-throw
    '(System.Drawing.Bitmap.)))

(deftest instance-properties
  (should-be
    [property instance-property]
    '(. (System.Drawing.Font. "Arial" 32) Bold)
    '(.Bold (System.Drawing.Font. "Arial" 32))
    '(.. (System.Drawing.Font. "Arial" 32) Bold)
    '(. (System.Drawing.Bitmap. 45 45) Size)
    '(.Size (System.Drawing.Bitmap. 45 45))
    '(.. (System.Drawing.Bitmap. 45 45) Size)
    '(. DateTime/Now Hour)
    '(.Hour DateTime/Now)
    '(.. DateTime/Now Hour)
    '(.. DateTime/Now (AddDays 89.4) (AddHours 56.7) Hour)))

(deftest instance-fields
  (should-be
    [field instance-field]
    '(. (System.IO.PathData.) Path)
    '(.Path (System.IO.PathData.))
    '(.. (System.IO.PathData.) Path)))

(deftest instance-methods
  (should-be
    [instance-method (exact :method)]
    '(. DateTime/Now (AddDays 89.4))
    '(.. DateTime/Now (AddDays 89.4))
    '(.AddDays DateTime/Now 89.4)
    '(.. DateTime/Now (AddDays 89.4) (AddHours 56.7))
    '(. DateTime/Now (ToBinary))
    '(.. DateTime/Now (ToBinary))
    '(. DateTime/Now ToBinary)
    '(.. DateTime/Now ToBinary)
    '(.ToBinary DateTime/Now)
    '(.. DateTime/Now (AddDays 89.4) (AddHours 56.7) ToBinary)
    '(. (System.Collections.Generic.List|[String]|.) (Add "Hello"))
    '(. (System.Collections.Generic.List|[Int64]|.) (Add 89))
    '(. (System.Collections.Generic.List|[System.Drawing.Point]|.)
      (Add (System.Drawing.Point. 6 7))))
  (should-be
    [instance-method (inexact :methods)]
    '(. DateTime/Now (AddMonths 89))
    '(.. DateTime/Now (AddMonths 89))
    '(.AddMonths DateTime/Now 89)
    '(. DateTime/Now (Subtract 89))
    '(.. DateTime/Now (Subtract 89))
    '(.Subtract DateTime/Now 89)
    '(. (System.Collections.Generic.List|[Int32]|.) (Add 89))
    '(. (System.Collections.Generic.List|[Int32]|.) (Add 89.5))
    '(. (System.Collections.Generic.List|[String]|.) (Add 89.5)))
  (should-throw
    '(.NotAMethod DateTime/Now)
    '(.NotAMethod DateTime/Now 99)
    '(. (System.Collections.Generic.List|[Int32]|.) (Add 89 78))))


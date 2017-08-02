(ns magic.tests
  (:refer-clojure :exclude [methods])
  (:import Activator IFn)
  (:require [mage.core :as il])
  (:use magic.core)
  (:use clojure.pprint
        clojure.test))

;; test gen
(defn types [asm]
  (->> asm
       assembly-load
       (.GetTypes)))

(defn methods [asm flags]
  (->> asm
       types
       (mapcat #(.GetMethods % flags))))

(defn properties [asm]
  (->> asm
       types
       (mapcat #(.GetProperties %))))

(defn fields [asm]
  (->> asm
       types
       (mapcat #(.GetFields %))))

(defn static-method-expr [method]
  (let [type (.DeclaringType method)
        name (.Name method)]
    (list 
      (symbol (str type "/" name))
      )
    ))

;; this will change when the magic api settles down
(defn compile-raw [bytecode]
  (let [asm-name "magic.tests"]
    (il/assembly
      asm-name
      (il/module
        (str asm-name ".dll")
        bytecode
        ))))

(defn compile-fn [expr]
  (let [asm-name "magic.tests"]
    (il/assembly
      asm-name
      (il/module
        (str asm-name ".dll")
        (compile (analyze expr) base-compilers)))))

(defn compile-fn [expr]
  (let [asm-name "magic.tests"]
    (-> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (compile (analyze expr) base-compilers)))
        il/emit!
        ; :mage.core/assembly-builder
        ; .GetTypes
        ; first
        )))

(defn magic-compile [expr]
  (compile-fn (list 'fn '[] expr)))

(defn magic-eval [expr]
  (.invoke (magic-compile expr)))

(defmacro test-same [name expr]
  `(deftest ~name
    (is (= (magic-eval (quote ~expr))
           ~expr))))

(defmacro test-not-nil [name expr]
  `(deftest ~name
    (is (not (nil? (magic-eval (quote ~expr)))))))

;; nil
(test-same strings nil)

;; strings
(test-same strings "foo")

;; boolean
(test-same strings true)
(test-same strings false)

;; numbers
(test-same integers 12)
(test-same negative-integers -12)
(test-same doubles- 12.3)
(test-same negative-doubles- -12.3)
(test-same ratios 1/2)
(test-same negative-ratios -1/2)
(test-same huge-ratios 999999999999999999999999999999999999/99999999999999999999999999999999999)
(test-same bigints-1 1N)
(test-same bigints-2 999999999999999999999999999999999999N)


;; chars
(test-same chars \f)

;; types
(test-same types-1 clojure.lang.Symbol)
(test-same types-2 System.Text.RegularExpressions.Regex)

;; lists
(test-same lists-1 '(1 2 3))
(test-same lists-2 '(1 "2" 3))
(test-same lists-3 '(1 (+ 1 2) 3))

;; vectors
(test-same vectors-numbers [1 2 3])
(test-same vectors-strings ["1" "2" "3"])
(test-same vectors-mixed ["1" 2 3.4])
(test-same vectors-exprs [(str "hello" 90) (+ 2 3.4)])

;; maps
(test-same maps-numbers {:a 1 :b 2 :c 3 })
(test-same maps-mixed {:a "1" :b "2" :c "3" })
(test-same maps-mixed {:a 1 :b "2" :c 3 })
(test-same maps-exprs {:a (str "hello" 90) :b (+ 2 3.4)})
(test-same maps-complex-keys {{:a (str "nice")} 1 (map inc (range 20)) "2"})

;; symbols
(test-same symbols 'foo)
(test-same symbols-namespace 'foo/bar)

;; keywords
(test-same keywords :foo)
(test-same keywords-namespaces :foo/bar)
(test-same keywords-double-colon ::foo)

;; invoke
(test-same invoke-str (str 1 2))
(test-same invoke-map-inc-range (map inc (range 20)))
(test-same invoke-map-lookup-1 (({:foo inc :bar dec} :foo) 10))
(test-same invoke-map-lookup-2 (({:foo inc :bar dec} :foo) ({:foo 10 :bar 9} :foo)))

;; regexp
(test-same regexp-1 (str #"foo")) ;; regexps arent values, is str the best test?  
(test-same regexp-2 (str #"foo(.*[a-z]+)")) ;; regexps arent values, is str the best test?  
(test-same regexp-works-1 (re-find #"bar" "foo bar baz"))
(test-same regexp-works-2 (re-find #"bar\b" "foo bar baz"))
(test-same regexp-works-3 (re-find #"bar\b" "foo barbaz"))

;; new expr
(test-not-nil new-object-1 (System.Xml.XmlDocument.))
(test-not-nil new-object-2 (System.IO.DirectoryInfo. "foo"))

(test-same new-valuetype-1 (System.Drawing.Point.))
(test-same new-valuetype-2 (System.Drawing.Point. 1 3))
(test-same new-valuetype-3 (System.Drawing.Point. 1.2 3.1))
(test-same new-valuetype-4 (System.Drawing.Point. 1.2 3))
(test-same new-valuetype-5 (System.Drawing.Point. (System.Drawing.Size. 5 6)))

;; instance property
(test-same instance-property (.Length (.GetFiles (System.IO.DirectoryInfo. "."))))
(test-same instance-property-valuetype (.X (System.Drawing.Point. 1 3)))

;; instance method
(test-same instance-method (.Length (.GetFiles (System.IO.DirectoryInfo. "."))))
(test-same instance-method-valuetype (.Offset (System.Drawing.Point. 1 3) 4 1))

;; static method
(test-same static-method-1 (Directory/Exists "."))
(test-same static-method-2 (.FullName (Directory/GetParent ".")))
(test-same static-method-valuetype (System.Drawing.Point/Add
                                     (System.Drawing.Point. 3 4)
                                     (System.Drawing.Size. 25 45)))

;; let bindings
(test-same let-valuetype (let [a 20] a))
(test-same let-reference (let [a "20"] a))
(test-same let-body (let [a "hello" b "world"] (str a b)))
(test-same let-body-valuetypes (let [a 1 b 2] (+ a b)))
(test-same let-nested
           (let [a 1]
             (let [b 2]
               (+ a b))))
(test-same let-nested-shadow
           (let [a 1]
             (let [b 2]
               (let [a 10]
                 (+ a b)))))
(test-same let-name-reuse-1
           (let [a 4
                 b (mapv inc (range a))
                 a "A Vector: "]
             (str a b)))
(test-same let-name-reuse-2
           (let [a 20
                 a (range a)
                 a (map inc a)
                 a (mapv str a)]
             a))
(test-same let-as-expr
           (map inc (let [a 8]
              (range a (let [b (long (count "Hello"))]
                         (+ a b))))))


(test-same let-vector-destructure (let [[a b c] [1 2 3]] (+ a b c)))
(test-same let-map-destructure (let [{:keys [a b c]} {:a 1 :b 2 :c 3}] (+ a b c)))

(run-tests)

(comment
  (binding [*compile-path* "."]
    (compile 'magic.reference))
  
  (binding [*test-out* *out*]
    (run-tests))
  )
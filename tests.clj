(ns magic.tests
  (:import Activator IFn)
  (:require [mage.core :as il] :reload)
  (:use magic.core :reload)
  (:use clojure.pprint
        clojure.test))

;; this will change when the magic api settles down
(defn symbolize-fn [expr]
  (let [asm-name (-> "magicTest" gensym str)]
    (il/assembly
      asm-name
      (il/module
        (str asm-name ".dll")
        (symbolize (analyze expr) base-symbolizers)))))

(defn compile-fn [expr]
  (let [asm-name (-> "magicTest" gensym str)]
    (-> (il/assembly
          asm-name
          (il/module
            (str asm-name ".dll")
            (symbolize (analyze expr) base-symbolizers)))
        il/emit!
        :mage.core/assembly-builder
        .GetTypes
        first)))

(defn magic-compile [expr]
  (compile-fn (list 'fn '[] expr)))

(defn magic-eval [expr]
  (let [^Type fn-type (magic-compile expr)
        ^IFn fn-instance (Activator/CreateInstance fn-type)]
    (.invoke fn-instance)))

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

(compile-fn '(fn [] clojure.lang.Symbol))
(compile-fn '(fn [] System.Text.RegularExpressions.Regex))
(compile-fn '(fn [] System.Int32))

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

(comment
  (-> '(fn [] clojure.lang.Symbol)
      analyze
      data-map
      :_methods
      first
      data-map
      :_body
      data-map
      :LastExpr
      data-map
      :Val
      type
      
      )
  
  (binding [*compile-path* "."]
    (compile 'magic.reference))
  
  (binding [*test-out* *out*]
    (run-tests))
  )
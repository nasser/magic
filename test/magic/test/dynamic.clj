(ns magic.test.dynamic
  (:use clojure.test magic.test.common))

;; using identity to drop type information

(deftest zero-arity-invocations
  (cljclr=magic
   (.Length (identity "hello"))
   (.GetType (identity "hello"))
   (.GetType (identity 90))
   (.GetType (identity 90.0))))

(deftest method-invocation
  (cljclr=magic
   (.Substring (identity "hello") 2 3)
   (.Substring (identity "hello") (int 2) (int 3))))


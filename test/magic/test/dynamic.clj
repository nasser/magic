(ns magic.test.dynamic
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

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
   (.Substring (identity "hello") 2.5 3.2)
   (.Substring (identity "hello") (int 2) (int 3))))

(deftest field-read
  (cljclr=magic
   (let [xx (identity (System.Security.Cryptography.CspParameters.))]
     (.KeyContainerName xx))
   (let [xx (identity (System.Security.Cryptography.CspParameters.))]
     (.KeyNumber xx))))

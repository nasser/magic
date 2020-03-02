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
     (.KeyNumber xx))
   (let [xx (identity (System.Security.Cryptography.CspParameters.))]
     (.-KeyContainerName xx))
   (let [xx (identity (System.Security.Cryptography.CspParameters.))]
     (.-KeyNumber xx))))

(deftest field-set
  (cljclr=magic
   (let [xx (identity (System.Security.Cryptography.CspParameters.))]
     (set! (.KeyContainerName xx) "Hello!")
     (.KeyContainerName xx))
   ;; works in MAGIC because we convert more consistently
   ;; fails in ClojureCLR because Int64 cannot be converted to Int32
   ;; in this context
   #_
     (let [xx (identity (System.Security.Cryptography.CspParameters.))]
       (set! (.KeyNumber xx) 99)
       (.KeyNumber xx))))
(ns magic.test.interop
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

(deftest static-methods
  (cljclr=magic
   (Math/Abs 90)
   (Math/Abs (Math/Sign 78))))

(deftest static-fields
  (cljclr=magic
   Math/PI
   Type/EmptyTypes))

(deftest static-properties
  (cljclr=magic
   System.Text.Encoding/ASCII))

(deftest zero-arity-instance-member
  (cljclr=magic
   (.Length "hello")
   (.GetType "hello")
   (.GetType 90)
   (.GetType 90.0)))

(deftest instance-method
  (cljclr=magic
   (.Substring "hello world" 2 4)
   (.Substring "hello world" (int 2) (int 4))))

(deftest instance-fields
  (let [xx (System.Security.Cryptography.CspParameters.)]
    (.KeyContainerName xx))
  (let [xx (System.Security.Cryptography.CspParameters.)]
    (.KeyNumber xx)))

(deftest constructor
  (cljclr=magic
   (Version. 1 2 3)
   (Version. (int 1) (int 2) (int 3))
   (Version.)
   (Guid. "ca761232ed4211cebacd00aa0057b223")))

(deftest init-object
  (cljclr=magic
   (Guid.)))

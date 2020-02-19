(ns magic.test.interop
  (:use clojure.test magic.test.common))

(deftest static-methods
  (cljclr=magic
   (Math/Abs 90)
   (Math/Abs (Math/Sign 78))))

(deftest static-fields
  (cljclr=magic
   Math/PI
   Type/EmptyTypes))

(deftest zero-arity-instance-member
  (cljclr=magic
   (.Length "hello")
   (.GetType "hello")
   (.GetType 90)
   (.GetType 90.0)))

(deftest instance-method
  (cljclr=magic
   (.Substring "hello world" 2 4)
   (.Substring "hello world" (int 2) (int 4))
   (.Substring "hello world" 2 4)))

(deftest constructor
  (cljclr=magic
   (Version. 1 2 3)
   (Version. (int 1) (int 2) (int 3))
   (Version.)
   (Guid. "ca761232ed4211cebacd00aa0057b223")))

(deftest init-object
  (cljclr=magic
   (Guid.)))

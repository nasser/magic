(ns magic.test.literals
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

 (deftest constants
   (cljclr=magic 2)
   (cljclr=magic 2.0)
   (cljclr=magic 2e9)
   (cljclr=magic "hello")
   (cljclr=magic :hello))

(deftest sets
  (cljclr=magic #{}))

(deftest maps
  (cljclr=magic {}))

(deftest lists
  (cljclr=magic []))
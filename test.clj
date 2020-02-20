(ns test
  (:require
   magic.test.literals
   magic.test.interop
   magic.test.dynamic
   magic.test.special)
  (:use clojure.test))

(defn all []
  (run-tests
   'magic.test.literals
   'magic.test.interop
   'magic.test.special
   'magic.test.dynamic))
(ns test
  (:require
   magic.test.literals
   magic.test.data-structures
   magic.test.string
   magic.test.logic
   magic.test.control
   magic.test.numbers
   magic.test.interop
   magic.test.dynamic
   magic.test.special
   magic.test.proxy
   magic.test.reify
   magic.test.fn
   magic.test.letfn)
  (:use clojure.test))

(defn all []
  (run-tests
   'magic.test.literals
   'magic.test.data-structures
   'magic.test.string
   'magic.test.logic
   'magic.test.control
   'magic.test.numbers
   'magic.test.interop
   'magic.test.special
   'magic.test.dynamic
   'magic.test.proxy
   'magic.test.reify
   'magic.test.fn
   'magic.test.letfn))

(defn run [& namespaces]
  (apply run-tests namespaces))
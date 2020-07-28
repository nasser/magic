;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Author: Paul M Bauer 
#_ ;; -nasser
(assembly-load-from "Clojure.Tests.dll")                                           ;;; DM: Added
(ns clojure.test-clojure.try-catch
  (:use clojure.test)
  #_ ;; -nasser
  (:import [clojure.test ReflectorTryCatchFixture 
                         ReflectorTryCatchFixture+Cookies]))                        ;;; ReflectorTryCatchFixture$Cookies

(defn maybe-unwrap-exception [e]
  (if (instance? System.Reflection.TargetInvocationException e)
    (.InnerException ^System.Reflection.TargetInvocationException e)
    e))

(defn- get-exception [expression]
  (try (eval expression)
    nil
    (catch System.Exception t                                                         ;;; java.lang.Throwable
      (maybe-unwrap-exception t))))

(deftest catch-receives-checked-exception
  (are [expression expected-exception] (= expected-exception
                                          (type (get-exception expression)))
    "Eh, I'm pretty safe" nil
    '(System.IO.StreamReader. "CAFEBABEx0/idonotexist") System.IO.DirectoryNotFoundException))            ;;; java.io.FileReader.   java.io.FileNotFoundException

(deftest try-catch-throw
  (is 1 (try 1))
  (is 90 (try 90 (catch Exception e 89)))
  (is 89 (try (throw (Exception. "oops")) 90 (catch Exception e 89)))
  (is 1 (try 1 (catch Exception e (clojure.lang.RT/load "hello"))))
  (is 2 (+ 1 (try 1 (catch Exception e 2))))
  (is 4 (+ 1 (do 2 (try 1 (catch Exception e 2)) 3)))
  (is 4 (+ 1 (do 2 (try (throw (Exception. "oops")) (catch Exception e 2)) 3))))


#_ ;; -nasser
(defn fail [x]
  (ReflectorTryCatchFixture/fail x))
#_ ;; -nasser
(defn make-instance []
  (ReflectorTryCatchFixture.))
#_ ;; -nasser
(deftest catch-receives-checked-exception-from-reflective-call
  (is (thrown-with-msg? ReflectorTryCatchFixture+Cookies #"Long" (fail 1)))                 ;;; ReflectorTryCatchFixture$Cookies
  (is (thrown-with-msg? ReflectorTryCatchFixture+Cookies #"Double" (fail 1.0)))             ;;; ReflectorTryCatchFixture$Cookies
  (is (thrown-with-msg? ReflectorTryCatchFixture+Cookies #"Wrapped"                         ;;; ReflectorTryCatchFixture$Cookies
        (.failWithCause (make-instance) 1.0))))
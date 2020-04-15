(ns magic.test.special
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

(deftest if-expr
  (cljclr=magic
   (if true 2 3)
   (if false 2 3)
   (if 1 2 3)
   (if "hello" 2 3)
   (if true 1)
   (if false 1)
   (if (< 10 1) 2 (clojure.lang.RT/load "run"))))

(deftest let-expr
  (cljclr=magic
   (let [a 1] a)
   (let [a 1 b 2] (+ a b))
   (let [c 90 a 1 b 2] (+ a b))
   (let [a 90 a (+ a 1) b (* a 2)] (+ a a))
   (let [a 1 b 2] (let [c (+ a b)] (* a b c)))))

(deftest if+let-expr
  (cljclr=magic
   (let [a true] (if a 3 4))
   (let [a false] (if a 3 4))
   (if true (let [a false] (if a 3 4)))
   (if false (let [a false] (if a 3 4)))
   (if 1 (let [a false] (if a 3 4)))
   (if "hello" (let [a false] (if a 3 4)))))

(deftest loop-expr
  (cljclr=magic
   (loop [a 0] a)
   (loop [a 100]
     (if (> 0 a)
       (recur (dec a))
       a))
   (loop [a 100]
     (if (> 0 a)
       (recur (dec a))
       (loop [a 100]
         (if (> 0 a)
           (recur (dec a))
           (loop [a 100]
             (if (> 0 a)
               (recur (dec a))
               a))))))))

(deftest do-expr
  (cljclr=magic
   (do)
   (do 1 2 3)
   (let [sb (System.Text.StringBuilder.)]
      (+
       (do 1 (.Append sb "hello") 2 (.Append sb "world") 99)
       (.Length (.ToString sb))))))


(deftest try-catch-throw
  (cljclr=magic
   (try 1)
   (try 90 (catch Exception e 89))
   (try (throw (Exception. "oops")) 90 (catch Exception e 89))
   (try 1 (catch Exception e (clojure.lang.RT/load "hello")))
   (try 1 (catch Exception e (throw e)))
   (type (try 1 (catch Exception e (throw e))))
   (+ 1 (try 1 (catch Exception e 2)))
   (+ 1 (do 2 (try 1 (catch Exception e 2)) 3))
   (+ 1 (do 2 (try (throw (Exception. "oops")) (catch Exception e 2)) 3))))

(deftest set!-expr
  (cljclr=magic
   (let [sb (System.Text.StringBuilder. "Hello World")]
     (set! (.Length sb) 3)
     (.ToString sb)))
  )

(deftest invoke-expr
  (cljclr=magic
   (inc 90)
   (inc (inc (inc 90)))
   (map inc (range 10))))

(deftest var-expr
  (cljclr=magic
   str
   +))

(deftest the-var-expr
  (cljclr=magic
   (var clojure.core/str)
   (var clojure.core/+)))

(deftest fn-expr
  (cljclr=magic
   (let [f (fn [x] (* x 2))]
     (f 80))
   (let [f (fn 
             ([x] (* x 2))
             ([x y] (* x y)))]
     (+ (f 80) (f 87 11)))
   (let [x 1
         y 2
         f (fn [z] (+ x y z))]
     (f 90))
   (let [x 1
         y 2
         f (fn [] (fn [z] (+ x y z)))
         g (f)]
     (g 90))))

;; this just tests that the monitor forms compile
;;it does not test their semantics!
(deftest monitors
  (cljclr=magic
   (let [lock (Object.)]
     (try
       (monitor-enter lock)
       90
       (finally
         (monitor-exit lock))))))

(deftest try-catch-finally-throw-exprs
  (cljclr=magic
   (try 1)
   (try 1 (catch Exception e 2))
   (try 1 (catch Exception e 2) (finally 3))
   (try
     (nth Type/EmptyTypes 7)
     (catch IndexOutOfRangeException e 45)
     (finally 99))
   (try
     (nth Type/EmptyTypes 7)
     (catch IndexOutOfRangeException e 45)
     (catch IndexOutOfRangeException e 46)
     (catch IndexOutOfRangeException e 47))
   (try
     (nth Type/EmptyTypes 7)
     (catch IndexOutOfRangeException e 45)
     (catch IndexOutOfRangeException e 46)
     (catch IndexOutOfRangeException e 47)
     (finally 99))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45)
     (finally 99))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45)
     (finally 99))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch Exception e 11)
     (catch Exception e 12)
     (catch Exception e 13)
     (catch IndexOutOfRangeException e 45))
   (try
     (nth Type/EmptyTypes 7)
     (catch Exception e 10)
     (catch Exception e 11)
     (catch Exception e 12)
     (catch Exception e 13)
     (catch IndexOutOfRangeException e 45)
     (finally 99))
   (try
     (nth Type/EmptyTypes 7)
     (catch IndexOutOfRangeException e 45)
     (catch Exception e 10))
   (try
     (nth Type/EmptyTypes 7)
     (catch IndexOutOfRangeException e 45)
     (catch Exception e 10)
     (finally 99))
   (try
     (throw (Exception.))
     (catch IndexOutOfRangeException e 45)
     (catch Exception e 10))
   (try
     (throw (Exception.))
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45))
   (try
     (try
       (try
         (throw (Exception.))
         (catch IndexOutOfRangeException e 45))
       (catch IndexOutOfRangeException e 45))
     (catch Exception e 10)
     (catch IndexOutOfRangeException e 45))))

(deftest quote-exprs
  (cljclr=magic
   (quote (+ 1 2 4 5))
   (quote (+ 1 "2" 4 "5"))
   (quote (+ a b c d))
   (quote [1 2 3 4])
   (quote ["1" 2 "3" 4])
   (quote [a b c d])
   (quote {:foo 1 :bar 2})
   (quote {:foo a :bar 2})
   (quote {a b c d})
   (quote #{1 2 3 4})
   (quote #{"1" 2 "3" 4})
   (quote #{a b c d})))

(deftest with-meta-exprs
  (cljclr=magic
   (let [xx (* 9 Math/PI) jj ^{:meta "data" :expr xx} [1 2 3 4]]
     jj)
   (let [xx (* 9 Math/PI) jj ^{:meta "data" :expr xx} [1 2 3 4]]
     (meta jj))
   (let [jj ^{:meta "data"} [1 2 3 4]] jj)
   (let [jj ^{:meta "data"} [1 2 3 4]] (meta jj))
   (let [jj ^{:meta "data"} {:foo 12}] jj)
   (let [jj ^{:meta "data"} {:foo 12}] (meta jj))))
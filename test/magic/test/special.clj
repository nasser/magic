(ns magic.test.special
  (:use clojure.test magic.test.common))

(deftest if-expr
  (cljclr=magic
   (if true 2 3)
   (if false 2 3)
   (if 1 2 3)
   (if "hello" 2 3)
   (if true 1)
   (if false 1)))

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
   (try 90 (catch Exception e 89))
   (try (throw (Exception. "oops")) 90 (catch Exception e 89))
   (try 1 (catch Exception e (clojure.lang.RT/load "hello")))
   (try 1 (catch Exception e (throw e)))
   (type (try 1 (catch Exception e (throw e)))) ) )

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
     (+ (f 80) (f 87 11)))))

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
(ns magic.test.fn
  (:require [clojure.test :refer [deftest]]
            [magic.api :as magic])
  (:use magic.test.common))

(deftest invocation
  (cljclr=magic
   (let [f (fn [x] x)]
     (f :hello))
   (let [f (fn [x] (+ x 10))]
     (f 8))))

;; clojureclr does not support most of these hints
;; so we cant compare outputs
(deftest primitive-type-hints
  (clojure.test/is
   (= 18 (magic/eval
          '(let [f (fn [^int x] (+ x 10))]
             (f 8)))))
  (clojure.test/is
   (= Int32
      (type
       (magic/eval
        '(let [f (fn [^int x] x)]
           (f 8))))))
  (clojure.test/is
   (= 18 (magic/eval
          '(let [f (fn [^long x] (+ x 10))]
             (f 8)))))
  (clojure.test/is
   (= Int64
      (type 
       (magic/eval
        '(let [f (fn [^long x] (+ x 10))]
           (f 8)))))))

(deftest closures
  (cljclr=magic
   (let [x 90
         f (fn [y] (+ x y))]
     (f 88))
   (let [x 90
         f (fn [y] (let [z 71]
                     (+ x y z)))]
     (f 88))))

(deftest higher-order
  (cljclr=magic
   (let [x 90
         f (fn [y] (fn [z] (+ x y z)))
         g (f 11)]
     (g 63))))

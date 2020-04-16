(ns magic.test.letfn
  (:require [clojure.test :refer [deftest]]
            [magic.api :as magic])
  (:use magic.test.common))

(deftest invocation
  (cljclr=magic
   (letfn [(twice [x]
             (* x 2))
           (six-times [y]
             (* (twice y) 3))]
     [(twice 15) (six-times 15)])))

(deftest mutual-recursion
  (cljclr=magic
   (letfn [(even2 [n] (neven? n))
           (neven? [n] (if (zero? n) true (nodd? (dec n))))
           (nodd? [n] (if (zero? n) false (neven? (dec n))))]
     [(even2 91) (even2 90)])))
(ns magic.test.reify
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

(deftest direct-interop
  (cljclr=magic
   (.count (reify clojure.lang.Counted
             (count [_] (int 9))))
   (let [p (reify clojure.lang.Counted
             (count [_] (int 8))
             (GetHashCode [_] 7))]
     (+ (.count p) (.GetHashCode p)))
   (let [p (reify clojure.lang.Counted
             (count [_] (int 8))
             (GetHashCode [self] (.count self)))]
     (+ (.count p) (.GetHashCode p)))
   ))

(deftest closures
  (cljclr=magic
   (let [x 90]
     (.count (reify clojure.lang.Counted
               (count [_] (int x)))))
   (let [f (fn [x] (reify clojure.lang.Counted
                     (count [_] (int x))))
         r (f 90)]
     (.count r))))

(deftest nested-proxy
  (cljclr=magic
   (.count (reify clojure.lang.Counted
             (count [self]
               (.count (reify clojure.lang.Counted
                         (count [self] (.count (reify clojure.lang.Counted
                                                 (count [self] 999)))))))))
   (let [x 78]
     (.count (reify clojure.lang.Counted
               (count [self]
                 (.count (reify clojure.lang.Counted
                           (count [self] (.count (reify clojure.lang.Counted
                                                   (count [self] (+ x 999)))))))))))
   (let [x 78
         y 12]
     (.count (reify clojure.lang.Counted
               (count [self]
                 (.count (reify clojure.lang.Counted
                           (count [self] (+ y (.count (reify clojure.lang.Counted
                                                        (count [self] (+ x 999))))))))))))))

(deftest metadata
  (cljclr=magic
   (let [r ^{:foo :bar}
         (reify clojure.lang.Counted
           (count [_] 9))]
     (meta r))
   (let [r ^{:foo :bar}
         (reify clojure.lang.Counted
           (count [_] 9))]
     [(meta r) (meta (with-meta r {:baz :qux}))])))

(deftest fallbacks
  (cljclr=magic
   (let [r (reify clojure.lang.Counted)]
     (try
       (.count r)
       ;; we throw a different exception type than ClojureCLR
       (catch Exception e
         8)))))

(deftest explicit-implementations
  (cljclr=magic
   (let [v (volatile! [])
         r (reify clojure.lang.ISeq
             (clojure.lang.ISeq.cons
               [self x] (vswap! v conj [:iseq x]) self)
             (clojure.lang.IPersistentCollection.cons
               [self x] (vswap! v conj [:ipc x]) self))
         ^clojure.lang.ISeq iseq r
         ^clojure.lang.IPersistentCollection ipc r]
     (.cons iseq :a)
     (.cons ipc :b)
     @v)))
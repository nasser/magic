(ns magic.test.control
  (:require [clojure.test :refer [deftest testing]])
  (:use magic.test.common))

(deftest test-do
  (are [x y] (= x y)
      ; no params => nil
    (do) nil

      ; return last
    (do 1) 1
    (do 1 2) 2
    (do 1 2 3 4 5) 5

      ; evaluate and return last
    (let [a (atom 0)]
      (do (reset! a (+ @a 1))   ; 1
          (reset! a (+ @a 1))   ; 2
          (reset! a (+ @a 1))   ; 3
          @a))  3)

  (are [x] (= (do x) x)
    nil
    false true
    0 42
    0.0 3.14
    2/3
    0M 1M
    \c
    "" "abc"
    'sym
    :kw
    () '(1 2)
    [] [1 2]
    {} {:a 1 :b 2}
    #{} #{1 2}))

(deftest test-loop
  (are [x y] (= x y)
    1 (loop []
        1)
    3 (loop [a 1]
        (if (< a 3)
          (recur (inc a))
          a))
    [2 4 6] (loop [a []
                   b [1 2 3]]
              (if (seq b)
                (recur (conj a (* 2 (first b)))
                       (next b))
                a))
    [6 4 2] (loop [a ()
                   b [1 2 3]]
              (if (seq b)
                (recur (conj a (* 2 (first b)))
                       (next b))
                a))))

(deftest test-when
  (are [x y] (= x y)
    1 (when true 1)
    nil (when true)
    nil (when false)
    nil (when false (throw (Exception.)))))

(deftest test-when-not
  (are [x y] (= x y)
    1 (when-not false 1)
    nil (when-not true)
    nil (when-not false)
    nil (when-not true (throw (Exception.)))))

(deftest test-if-not
  (are [x y] (= x y)
    1 (if-not false 1)
    1 (if-not false 1 (throw (Exception.)))
    nil (if-not true 1)
    2 (if-not true 1 2)
    nil (if-not true (throw (Exception.)))
    1 (if-not true (throw (Exception.)) 1)))

(deftest test-when-let
  (are [x y] (= x y)
    1 (when-let [a 1]
        a)
    2 (when-let [[a b] '(1 2)]
        b)
    nil (when-let [a false]
          (throw (Exception.)))))

(deftest test-if-let
  (are [x y] (= x y)
    1 (if-let [a 1]
        a)
    2 (if-let [[a b] '(1 2)]
        b)
    nil (if-let [a false]
          (throw (Exception.)))
    1 (if-let [a false]
        a 1)
    1 (if-let [[a b] nil]
        b 1)
    1 (if-let [a false]
        (throw (Exception.))
        1)))

(deftest test-when-first
  (are [x y] (= x y)
    1 (when-first [a [1 2]]
        a)
    2 (when-first [[a b] '((1 2) 3)]
        b)
    nil (when-first [a nil]
          (throw (Exception.)))))

(deftest test-if-some
  (are [x y] (= x y)
    1 (if-some [a 1] a)
    false (if-some [a false] a)
    nil (if-some [a nil] (throw (Exception.)))
    3 (if-some [[a b] [1 2]] (+ a b))
    1 (if-some [[a b] nil] b 1)
    1 (if-some [a nil] (throw (Exception.)) 1)))

(deftest test-when-some
  (are [x y] (= x y)
    1 (when-some [a 1] a)
    2 (when-some [[a b] [1 2]] b)
    false (when-some [a false] a)
    nil (when-some [a nil] (throw (Exception.)))))

(deftest test-cond
  (are [x y] (= x y)
    (cond) nil

    (cond nil true) nil
    (cond false true) nil

    (cond true 1 true (throw (Exception.))) 1
    (cond nil 1 false 2 true 3 true 4) 3
    (cond nil 1 false 2 true 3 true (throw (Exception.))) 3)

  ; false
  (are [x]  (= (cond x :a true :b) :b)
    nil false)

  ; true
  (are [x]  (= (cond x :a true :b) :a)
    true
    0 42
    0.0 3.14
    2/3
    0M 1M
    \c
    "" "abc"
    'sym
    :kw
    () '(1 2)
    [] [1 2]
    {} {:a 1 :b 2}
    #{} #{1 2})

  ; evaluation
  (are [x y] (= x y)
    (cond (> 3 2) (+ 1 2) true :result true (throw (Exception.))) 3
    (cond (< 3 2) (+ 1 2) true :result true (throw (Exception.))) :result)

  (are [x] (= (cond true x) x)
    nil
    false true
    0 42
    0.0 3.14
    2/3
    0M 1M
    \c
    "" "abc"
    'sym
    :kw
    () '(1 2)
    [] [1 2]
    {} {:a 1 :b 2}
    #{} #{1 2}))


(deftest test-condp
  (are [x] (= :pass x)
    (condp = 1
      1 :pass
      2 :fail)
    (condp = 1
      2 :fail
      1 :pass)
    (condp = 1
      2 :fail
      :pass)
    (condp = 1
      :pass)
    (condp = 1
      2 :fail
         ;; doc of condp says result-expr is returned
         ;; shouldn't it say similar to cond: "evaluates and returns
         ;; the value of the corresponding expr and doesn't evaluate any of the
         ;; other tests or exprs."
      (identity :pass))
    (condp + 1
      1 :>> #(if (= % 2) :pass :fail))
    (condp + 1
      1 :>> #(if (= % 3) :fail :pass)))
  )

(deftest test-dotimes
  ;; dotimes always returns nil
  (is (= nil (dotimes [n 1] n)))
  ;; test using an atom since dotimes is for modifying
  ;; test executes n times
  (is (= 3
         (let [a (atom 0)]
           (dotimes [n 3]
             (swap! a inc))
           @a)))
  ;; test all values of n
  (is (= [0 1 2]
         (let [a (atom [])]
           (dotimes [n 3]
             (swap! a conj n))
           @a)))
  (is (= []
         (let [a (atom [])]
           (dotimes [n 0]
             (swap! a conj n))
           @a))))

(deftest test-while
  (is (= nil (while nil (throw (Exception. "never")))))
  (is (= [0 nil]
         ;; a will dec to 0
         ;; while always returns nil
         (let [a (atom 3)
               w (while (pos? @a)
                   (swap! a dec))]
           [@a w]))))

(deftest test-case
  (testing "can match many kinds of things"
    (are [result input]
         (= result (let [two 2] 
                     (case input
                       1 :number
                       "foo" :string
                       \a :char
                       pow :symbol
                       :zap :keyword
                       (2 \b "bar") :one-of-many
                       [1 2] :sequential-thing
                       {:a 2} :map
                       {:r 2 :d 2} :droid
                       #{2 3 4 5} :set
                       [1 [[[2]]]] :deeply-nested
                       nil :nil
                       :default)))
      :number 1
      :string "foo"
      :char \a
      :keyword :zap
      :symbol 'pow
      :one-of-many 2
      :one-of-many \b
      :one-of-many "bar"
      :sequential-thing [1 2]
      :sequential-thing (list 1 2)
      :sequential-thing [1 two]
      :map {:a 2}
      :map {:a two}
      :set #{2 3 4 5}
      :set #{two 3 4 5}
      :default #{2 3 4 5 6}
      :droid {:r 2 :d 2}
      :deeply-nested [1 [[[two]]]]
      :nil nil
      :default :anything-not-appearing-above))
  (testing "sorting doesn't matter"
    (are [result input]
         (= result (case input
                     {:b 2 :a 1} :map
                     #{3 2 1} :set
                     :default))
      :map {:a 1 :b 2}
      :map (sorted-map :a 1 :b 2)
      :set #{3 2 1}
      :set (sorted-set 2 1 3)))
  (testing "test number equivalence"
    (is (= :1 (case 1N 1 :1 :else))))
   (testing "test warn when boxing/hashing expr for all-ints case"
  #_  (should-print-err-message
     #"Performance warning, .*:\d+ - case has int tests, but tested expression is not primitive..*\r?\n"
     (let [x (Object.)] (case x 1 1 2))))
  (testing "test correct behavior on sparse ints"
    (are [result input] (= result (case input
                                    2r1000000000000000000000000000000 :big
                                    1 :small
                                    :else))
      :small 1
      :big 1073741824
      :else 2)
    (are [result input] (= result (case input
                                    1 :small
                                    2r1000000000000000000000000000000 :big
                                    :else))
      :small 1
      :big 1073741824
      :else 2))
  #_(testing "test emits return types"
    (should-not-reflect (clojure.lang.BigDecimal/Create (case 1 1 1)))                   ;;; (Long. (case 1 1 1)) ; new Long(long)
    (should-not-reflect (clojure.lang.BigDecimal/Create (case 1 1 "1"))))                ;;; (Long. (case 1 1 "1")) ; new Long(String)
  (testing "non-equivalence of chars and nums"
    (are [result input] (= result (case input 97 :97 :else))
      :else \a
      :else (char \a)
      :97 (int \a))
    (are [result input] (= result (case input \a :a :else))
      :else 97
      :else 97N
      :a (char 97)))
  (testing "test correct behaviour on Number truncation"
    (is (let [^Object x (identity 8589934591)]
          (= :diff (case x -1 :oops :diff))))
    (is (let [^Object y (identity -1)]
          (= :same (case y -1 :same :oops)))))
  (testing "test correct behavior on hash collision"
    ;; case uses Java .hashCode to put values into hash buckets.
    (is (== (.GetHashCode 1) (.GetHashCode 9223372039002259457N)))        ;;; .hashCode .hashCode
    (are [result input] (= result (case input
                                    1 :long
                                    9223372039002259457N :big
                                    :else))
      :long 1
      :big 9223372039002259457N
      :else 4294967296
      :else 2)
    (are [result input] (= result (case input
                                    9223372039002259457N :big
                                    1 :long
                                    :else))
      :long 1
      :big 9223372039002259457N
      :else 4294967296
      :else 2)
    (are [result input] (= result (case input
                                    0 :zero
                                    -1 :neg1
                                    2 :two
                                    :oops :OOPS))
      :zero 0
      :neg1 -1
      :two 2
      :OOPS :oops)
    (are [result input] (= result (case input
                                    1204766517646190306 :a
                                    1 :b
                                    -2 :c
                                    :d))
      :a 1204766517646190306
      :b 1
      :c -2
      :d 4294967296
      :d 3))
  (testing "test warn for hash collision"
    #_ (should-print-err-message
     #"Performance warning, .*:\d+ - hash collision of some case test constants; if selected, those entries will be tested sequentially..*\r?\n"
     (case 1 1 :long 9223372039002259457N :big 2)))
  (testing "test constants are *not* evaluated"
    (are [result input] (= result (case input
                                    (throw (Exception. "boom")) :piece-of-throw-expr         ;;; RuntimeException
                                    :no-match))
      :piece-of-throw-expr 'throw
      :piece-of-throw-expr '[Exception. "boom"]               ;;; RuntimeException
      :no-match nil)))
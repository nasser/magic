(ns magic.test.numbers
  (:require [clojure.test :refer [deftest testing]])
  (:use magic.test.common))

(deftest unchecked-cast-char
  ; in keeping with the checked cast functions, char and Character can only be cast to int
  (is (unchecked-int (char 0xFFFF)))
  (is (let [c (char 0xFFFF)] (unchecked-int c))))

(defonce DELTA 1e-12)

(deftest test-add
  (are [x y] (= x y)
    (+) 0
    (+ 1) 1
    (+ 1 2) 3
    (+ 1 2 3) 6

    (+ -1) -1
    (+ -1 -2) -3
    (+ -1 +2 -3) -2

    (+ 1 -1) 0
    (+ -1 1) 0

    (+ 2/3) 2/3
    (+ 2/3 1) 5/3
    (+ 2/3 1/3) 1)

  (are [x y] (< (- x y) 1e-12)
    (+ 1.2) 1.2
    (+ 1.1 2.4) 3.5
    (+ 1.1 2.2 3.3) 6.6)

  (is (> (+ Int32/MaxValue 10) Int32/MaxValue))  ; no overflow
)    ; no string concatenation


(deftest test-subtract
  (are [x y] (= x y)
    (- 1) -1
    (- 1 2) -1
    (- 1 2 3) -4

    (- -2) 2
    (- 1 -2) 3
    (- 1 -2 -3) 6

    (- 1 1) 0
    (- -1 -1) 0

    (- 2/3) -2/3
    (- 2/3 1) -1/3
    (- 2/3 1/3) 1/3)

  (are [x y] (< (- x y) 1e-12)
    (- 1.2) -1.2
    (- 2.2 1.1) 1.1
    (- 6.6 2.2 1.1) 3.3)

  (is (< (- Int32/MinValue 10) Int32/MinValue)))  ; no underflow


(deftest test-multiply
  (are [x y] (= x y)
    (*) 1
    (* 2) 2
    (* 2 3) 6
    (* 2 3 4) 24

    (* -2) -2
    (* 2 -3) -6
    (* 2 -3 -1) 6

    (* 1/2) 1/2
    (* 1/2 1/3) 1/6
    (* 1/2 1/3 -1/4) -1/24)

  (are [x y] (< (- x y) 1e-12)
    (* 1.2) 1.2
    (* 2.0 1.2) 2.4
    (* 3.5 2.0 1.2) 8.4)

  (is (> (* 3 (int (/ Int32/MaxValue 2.0))) Int32/MaxValue)))  ; no overflow

(deftest test-multiply-longs-at-edge
  (are [x] (= x 9223372036854775808N)
    (*' -1 Int64/MinValue)
    (*' Int64/MinValue -1)
    (* -1N Int64/MinValue)
    (* Int64/MinValue -1N)
    (* -1 (bigint Int64/MinValue))
    (* (bigint Int64/MinValue) -1)))

(deftest test-ratios-simplify-to-ints-where-appropriate
  (testing "negative denominator (assembla #275)"
    (is (integer? (/ 1 -1/2)))
    (is (integer? (/ 0 -1/2)))))

(deftest test-divide
  (are [x y] (= x y)
    (/ 1) 1
    (/ 2) 1/2
    (/ 3 2) 3/2
    (/ 4 2) 2
    (/ 24 3 2) 4
    (/ 24 3 2 -1) -4

    (/ -1) -1
    (/ -2) -1/2
    (/ -3 -2) 3/2
    (/ -4 -2) 2
    (/ -4 2) -2)

  (are [x y] (< (- x y) 1e-12)
    (/ 4.5 3) 1.5
    (/ 4.5 3.0 3.0) 0.5))

(deftest test-divide-bigint-at-edge
  (are [x] (= x (-' Int64/MinValue))
    (/ Int64/MinValue -1N)
    (/ (bigint Int64/MinValue) -1)
    (/ (bigint Int64/MinValue) -1N)
    (quot Int64/MinValue -1N)
    (quot (bigint Int64/MinValue) -1)
    (quot (bigint Int64/MinValue) -1N)))


(deftest test-mod
  (are [x y] (= x y)
    (mod 4 2) 0
    (mod 3 2) 1
    (mod 6 4) 2
    (mod 0 5) 0

    (mod 2 1/2) 0
    (mod 2/3 1/2) 1/6
    (mod 1 2/3) 1/3

    (mod 4.0 2.0) 0.0
    (mod 4.5 2.0) 0.5

    ; |num| > |div|, num != k * div
    (mod 42 5) 2      ; (42 / 5) * 5 + (42 mod 5)        = 8 * 5 + 2        = 42
    (mod 42 -5) -3    ; (42 / -5) * (-5) + (42 mod -5)   = -9 * (-5) + (-3) = 42
    (mod -42 5) 3     ; (-42 / 5) * 5 + (-42 mod 5)      = -9 * 5 + 3       = -42
    (mod -42 -5) -2   ; (-42 / -5) * (-5) + (-42 mod -5) = 8 * (-5) + (-2)  = -42

    ; |num| > |div|, num = k * div
    (mod 9 3) 0       ; (9 / 3) * 3 + (9 mod 3) = 3 * 3 + 0 = 9
    (mod 9 -3) 0
    (mod -9 3) 0
    (mod -9 -3) 0

    ; |num| < |div|
    (mod 2 5) 2       ; (2 / 5) * 5 + (2 mod 5)        = 0 * 5 + 2          = 2
    (mod 2 -5) -3     ; (2 / -5) * (-5) + (2 mod -5)   = (-1) * (-5) + (-3) = 2
    (mod -2 5) 3      ; (-2 / 5) * 5 + (-2 mod 5)      = (-1) * 5 + 3       = -2
    (mod -2 -5) -2    ; (-2 / -5) * (-5) + (-2 mod -5) = 0 * (-5) + (-2)    = -2

    ; num = 0, div != 0
    (mod 0 3) 0       ; (0 / 3) * 3 + (0 mod 3) = 0 * 3 + 0 = 0
    (mod 0 -3) 0

    ; large args
    (mod 3216478362187432 432143214) 120355456))

;; rem & quot
;; http://en.wikipedia.org/wiki/Remainder

(deftest test-rem
  (are [x y] (= x y)
    (rem 4 2) 0
    (rem 3 2) 1
    (rem 6 4) 2
    (rem 0 5) 0

    (rem 2 1/2) 0
    (rem 2/3 1/2) 1/6
    (rem 1 2/3) 1/3

    (rem 4.0 2.0) 0.0
    (rem 4.5 2.0) 0.5

    ; |num| > |div|, num != k * div
    (rem 42 5) 2      ; (8 * 5) + 2 == 42
    (rem 42 -5) 2     ; (-8 * -5) + 2 == 42
    (rem -42 5) -2    ; (-8 * 5) + -2 == -42
    (rem -42 -5) -2   ; (8 * -5) + -2 == -42

    ; |num| > |div|, num = k * div
    (rem 9 3) 0
    (rem 9 -3) 0
    (rem -9 3) 0
    (rem -9 -3) 0

    ; |num| < |div|
    (rem 2 5) 2
    (rem 2 -5) 2
    (rem -2 5) -2
    (rem -2 -5) -2

    ; num = 0, div != 0
    (rem 0 3) 0
    (rem 0 -3) 0))

(deftest test-quot
  ; wrong number of args
  ; divide by zero
  (are [x y] (= x y)
    (quot 4 2) 2
    (quot 3 2) 1
    (quot 6 4) 1
    (quot 0 5) 0

    (quot 2 1/2) 4
    (quot 2/3 1/2) 1
    (quot 1 2/3) 1

    (quot 4.0 2.0) 2.0
    (quot 4.5 2.0) 2.0

    ; |num| > |div|, num != k * div
    (quot 42 5) 8     ; (8 * 5) + 2 == 42
    (quot 42 -5) -8   ; (-8 * -5) + 2 == 42
    (quot -42 5) -8   ; (-8 * 5) + -2 == -42
    (quot -42 -5) 8   ; (8 * -5) + -2 == -42

    ; |num| > |div|, num = k * div
    (quot 9 3) 3
    (quot 9 -3) -3
    (quot -9 3) -3
    (quot -9 -3) 3

    ; |num| < |div|
    (quot 2 5) 0
    (quot 2 -5) 0
    (quot -2 5) 0
    (quot -2 -5) 0

    ; num = 0, div != 0
    (quot 0 3) 0
    (quot 0 -3) 0))

#_(deftest test-pos?-zero?-neg?
    (let [nums [[(sbyte 2) (sbyte 0) (sbyte -1)]
                [(short 3) (short 0) (short -3)]
                [(int 4) (int 0) (int -4)]
                [(long 5) (long 0) (long -5)]
                [(bigint 6) (bigint 0) (bigint -6)]
                [(float 7) (float 0) (float -7)]
                [(double 8) (double 0) (double -8)]
                [(bigdec 9) (bigdec 0) (bigdec -9)]
                [2/3 0 -2/3]]
          pred-result [[pos?  [true false false]]
                       [zero? [false true false]]
                       [neg?  [false false true]]]]
      (doseq [pr pred-result]
        (doseq [n nums]
          (is (= (map (first pr) n) (second pr))
              (pr-str (first pr) n))))))


;; even? odd?

(deftest test-even?
  (are [x] (true? x)
    (even? -4)
    (not (even? -3))
    (even? 0)
    (not (even? 5))
    (even? 8)))

(deftest test-odd?
  (are [x] (true? x)
    (not (odd? -4))
    (odd? -3)
    (not (odd? 0))
    (odd? 5)
    (not (odd? 8))))


(defn- expt
  "clojure.contrib.math/expt is a better and much faster impl, but this works.
Math/pow overflows to Infinity."
  [x n] (apply *' (replicate n x)))

(deftest test-bit-shift-left
  (are [x y] (= x y)
    2r10 (bit-shift-left 2r1 1)
    2r100 (bit-shift-left 2r1 2)
    2r1000 (bit-shift-left 2r1 3)
    2r00101110 (bit-shift-left 2r00010111 1)
    2r00101110 (apply bit-shift-left [2r00010111 1])
    0 (bit-shift-left 2r10 -1) ; truncated to least 6-bits, 63
    (apply *' (replicate 2 32)) (bit-shift-left 1 32)
    (apply *' (replicate 2 16)) (bit-shift-left 1 10000) ; truncated to least 6-bits, 16
    ))

(deftest test-bit-shift-right
  (are [x y] (= x y)
    2r0 (bit-shift-right 2r1 1)
    2r010 (bit-shift-right 2r100 1)
    2r001 (bit-shift-right 2r100 2)
    2r000 (bit-shift-right 2r100 3)
    2r0001011 (bit-shift-right 2r00010111 1)
    2r0001011 (apply bit-shift-right [2r00010111 1])
    0 (bit-shift-right 2r10 -1) ; truncated to least 6-bits, 63
    1 (bit-shift-right (apply *' (replicate 2 32)) 32)
    1 (bit-shift-right (apply *' (replicate 2 16)) 10000) ; truncated to least 6-bits, 16
    -1 (bit-shift-right -2r10 1)))

(deftest test-unsigned-bit-shift-right
  (are [x y] (= x y)
    2r0 (unsigned-bit-shift-right 2r1 1)
    2r010 (unsigned-bit-shift-right 2r100 1)
    2r001 (unsigned-bit-shift-right 2r100 2)
    2r000 (unsigned-bit-shift-right 2r100 3)
    2r0001011 (unsigned-bit-shift-right 2r00010111 1)
    2r0001011 (apply unsigned-bit-shift-right [2r00010111 1])
    0 (unsigned-bit-shift-right 2r10 -1) ; truncated to least 6-bits, 63
    1 (unsigned-bit-shift-right (apply *' (replicate 2 32)) 32)
    1 (unsigned-bit-shift-right (apply *' (replicate 2 16)) 10000) ; truncated to least 6-bits, 16
    9223372036854775807 (unsigned-bit-shift-right -2r10 1)))

(deftest test-bit-clear
  (is (= 2r1101 (bit-clear 2r1111 1)))
  (is (= 2r1101 (bit-clear 2r1101 1))))

(deftest test-bit-set
  (is (= 2r1111 (bit-set 2r1111 1)))
  (is (= 2r1111 (bit-set 2r1101 1))))

(deftest test-bit-flip
  (is (= 2r1101 (bit-flip 2r1111 1)))
  (is (= 2r1111 (bit-flip 2r1101 1))))

(deftest test-bit-test
  (is (true? (bit-test 2r1111 1)))
  (is (false? (bit-test 2r1101 1))))

(deftest test-array-types
  (are [x y z] (= (clojure.lang.RT/classForName x) (class y) (class z))
    "System.Boolean[]" (boolean-array 1) (booleans (boolean-array 1 true))
    "System.Byte[]"    (byte-array 1) (bytes (byte-array 1 (byte 1)))
    "System.Char[]"    (char-array 1) (chars (char-array 1 \a))
    "System.Int16[]"   (short-array 1) (shorts (short-array 1 (short 1)))
    "System.Single[]"  (float-array 1) (floats (float-array 1 1))
    "System.Double[]"  (double-array 1) (doubles (double-array 1 1))
    "System.Int32[]"   (int-array 1) (ints (int-array 1 1))
    "System.Int64[]"   (long-array 1) (longs (long-array 1 1))))


(deftest test-ratios
  (is (== (denominator 1/2) 2))
  (is (== (numerator 1/2) 1))
  (is (= (bigint (/ 100000000000000000000 3)) 33333333333333333333))
  (is (= (long 10000000000000000000/3) 3333333333333333333)))

(deftest test-arbitrary-precision-subtract
  (are [x y] (= x y)
    9223372036854775808N (-' 0 -9223372036854775808)
    clojure.lang.BigInt  (class (-' 0 -9223372036854775808))
    Int64      (class (-' 0 -9223372036854775807))))

(deftest test-min-max
  (testing "min/max on different numbers of floats and doubles"
    (are [xmin xmax a]
         (and (= (float xmin) (min (float a)))
              (= (float xmax) (max (float a)))
              (= xmin (min a))
              (= xmax (max a)))
      0.0 0.0 0.0)
    (are [xmin xmax a b]
         (and (= (float xmin) (min (float a) (float b)))
              (= (float xmax) (max (float a) (float b)))
              (= xmin (min a b))
              (= xmax (max a b)))
      -1.0  0.0  0.0 -1.0
      -1.0  0.0 -1.0  0.0
      0.0  1.0  0.0  1.0
      0.0  1.0  1.0  0.0)
    (are [xmin xmax a b c]
         (and (= (float xmin) (min (float a) (float b) (float c)))
              (= (float xmax) (max (float a) (float b) (float c)))
              (= xmin (min a b c))
              (= xmax (max a b c)))
      -1.0  1.0  0.0  1.0 -1.0
      -1.0  1.0  0.0 -1.0  1.0
      -1.0  1.0 -1.0  1.0  0.0))
  (testing "min/max preserves type of winner"
    (is (= Int64 (class (max 10))))
    (is (= Int64 (class (max 1.0 10))))
    (is (= Int64 (class (max 10 1.0))))
    (is (= Int64 (class (max 10 1.0 2.0))))
    (is (= Int64 (class (max 1.0 10 2.0))))
    (is (= Int64 (class (max 1.0 2.0 10))))
    (is (= Double (class (max 1 2 10.0 3 4 5))))
    (is (= Int64 (class (min 10))))
    (is (= Int64 (class (min 1.0 -10))))
    (is (= Int64 (class (min -10 1.0))))
    (is (= Int64 (class (min -10 1.0 2.0))))
    (is (= Int64 (class (min 1.0 -10 2.0))))
    (is (= Int64 (class (min 1.0 2.0 -10))))
    (is (= Double (class (min 1 2 -10.0 3 4 5))))))

(deftest comparisons
  (are [small big] (< small big)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< big small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< small small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (< (int small) (int big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< (int big) (int small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< (int small) (int small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (< (double small) (double big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< (double big) (double small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (< (double small) (double small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= small big)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= small small)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (<= big small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= (int small) (int big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= (int small) (int small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (<= (int big) (int small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= (double small) (double big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (<= (double small) (double small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (<= (double big) (double small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (> big small)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> small big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> small small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (> (int big) (int small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> (int small) (int big)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> (int small) (int small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (> (double big) (double small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> (double small) (double big)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (> (double small) (double small)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= big small)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= small small)
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (>= small big))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= (int big) (int small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= (int small) (int small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (>= (int small) (int big)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= (double big) (double small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (>= (double small) (double small))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M)
  (are [small big] (not (>= (double small) (double big)))
    1 10 1.0 10.0 (int 1) (int 10) (float 1) (float 10.0) 9/10 99/10 1N 10N 1M 10M))

(deftest test-nan-comparison
  (are [x y] (= x y)
    (< 1000 Double/NaN) (< 1000 (double Double/NaN))
    (<= 1000 Double/NaN) (<= 1000 (double Double/NaN))
    (> 1000 Double/NaN) (> 1000 (double Double/NaN))
    (>= 1000 Double/NaN) (>= 1000 (double Double/NaN))))

(deftest test-nan-as-operand
  (are [x] (Double/IsNaN x)                                    ;;; Double/isNaN
    (+ Double/NaN 1)
    (+ Double/NaN 0)
    (+ Double/NaN 0.0)
    (+ 1 Double/NaN)
    (+ 0 Double/NaN)
    (+ 0.0 Double/NaN)
    (+ Double/NaN Double/NaN)
    (- Double/NaN 1)
    (- Double/NaN 0)
    (- Double/NaN 0.0)
    (- 1 Double/NaN)
    (- 0 Double/NaN)
    (- 0.0 Double/NaN)
    (- Double/NaN Double/NaN)
    (* Double/NaN 1)
    (* Double/NaN 0)
    (* Double/NaN 0.0)
    (* 1 Double/NaN)
    (* 0 Double/NaN)
    (* 0.0 Double/NaN)
    (* Double/NaN Double/NaN)
    (/ Double/NaN 1)
    (/ Double/NaN 0)
    (/ Double/NaN 0.0)
    (/ 1 Double/NaN)
    (/ 0 Double/NaN)
    (/ 0.0 Double/NaN)
    (/ Double/NaN Double/NaN)
    (+ (cast Object Double/NaN) 1)
    (+ (cast Object Double/NaN) 0)
    (+ (cast Object Double/NaN) 0.0)
    (+ 1 (cast Object Double/NaN))
    (+ 0 (cast Object Double/NaN))
    (+ 0.0 (cast Object Double/NaN))
    (+ (cast Object Double/NaN) (cast Object Double/NaN))
    (- (cast Object Double/NaN) 1)
    (- (cast Object Double/NaN) 0)
    (- (cast Object Double/NaN) 0.0)
    (- 1 (cast Object Double/NaN))
    (- 0 (cast Object Double/NaN))
    (- 0.0 (cast Object Double/NaN))
    (- (cast Object Double/NaN) (cast Object Double/NaN))
    (* (cast Object Double/NaN) 1)
    (* (cast Object Double/NaN) 0)
    (* (cast Object Double/NaN) 0.0)
    (* 1 (cast Object Double/NaN))
    (* 0 (cast Object Double/NaN))
    (* 0.0 (cast Object Double/NaN))
    (* (cast Object Double/NaN) (cast Object Double/NaN))
    (/ (cast Object Double/NaN) 1)
    (/ (cast Object Double/NaN) 0)
    (/ (cast Object Double/NaN) 0.0)
    (/ 1 (cast Object Double/NaN))
    (/ 0 (cast Object Double/NaN))
    (/ 0.0 (cast Object Double/NaN))
    (/ (cast Object Double/NaN) (cast Object Double/NaN))
    (+ Double/NaN (cast Object Double/NaN))
    (+ (cast Object Double/NaN) Double/NaN)
    (- Double/NaN (cast Object Double/NaN))
    (- (cast Object Double/NaN) Double/NaN)
    (* Double/NaN (cast Object Double/NaN))
    (* (cast Object Double/NaN) Double/NaN)
    (/ Double/NaN (cast Object Double/NaN))
    (/ (cast Object Double/NaN) Double/NaN)))
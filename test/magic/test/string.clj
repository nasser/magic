(ns magic.test.string
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

(deftest t-split
  (is (= ["a" "b"] (clojure.string/split "a-b" #"-")))
  (is (= ["a" "b-c"] (clojure.string/split "a-b-c" #"-" 2)))
  (is (vector? (clojure.string/split "abc" #"-"))))

(deftest t-reverse
  (is (= "tab" (clojure.string/reverse "bat"))))

(deftest t-replace
  (is (= "faabar" (clojure.string/replace "foobar" \o \a)))
  (is (= "foobar" (clojure.string/replace "foobar" \z \a)))
  (is (= "barbarbar" (clojure.string/replace "foobarfoo" "foo" "bar")))
  (is (= "foobarfoo" (clojure.string/replace "foobarfoo" "baz" "bar")))
  (is (= "f$$d" (clojure.string/replace "food" "o" "$")))
  (is (= "f\\\\d" (clojure.string/replace "food" "o" "\\")))
  (is (= "barbarbar" (clojure.string/replace "foobarfoo" #"foo" "bar")))
  (is (= "foobarfoo" (clojure.string/replace "foobarfoo" #"baz" "bar")))
  (is (= "f$$d" (clojure.string/replace "food" #"o" (clojure.string/re-quote-replacement "$"))))
  (is (= "f\\\\d" (clojure.string/replace "food" #"o" (clojure.string/re-quote-replacement "\\"))))
  (is (= "FOObarFOO" (clojure.string/replace "foobarfoo" #"foo" clojure.string/upper-case)))
  (is (= "foobarfoo" (clojure.string/replace "foobarfoo" #"baz" clojure.string/upper-case)))
  (is (= "OObarOO" (clojure.string/replace "foobarfoo" #"f(o+)" (fn [[m g1]] (clojure.string/upper-case g1)))))
  (is (= "baz\\bang\\" (clojure.string/replace "bazslashbangslash" #"slash" (constantly "\\")))))

(deftest t-replace-first
  (is (= "faobar" (clojure.string/replace-first "foobar" \o \a)))
  (is (= "foobar" (clojure.string/replace-first "foobar" \z \a)))
  (is (= "z.ology" (clojure.string/replace-first "zoology" \o \.)))
  (is (= "barbarfoo" (clojure.string/replace-first "foobarfoo" "foo" "bar")))
  (is (= "foobarfoo" (clojure.string/replace-first "foobarfoo" "baz" "bar")))
  (is (= "f$od" (clojure.string/replace-first "food" "o" "$")))
  (is (= "f\\od" (clojure.string/replace-first "food" "o" "\\")))
  (is (= "barbarfoo" (clojure.string/replace-first "foobarfoo" #"foo" "bar")))
  (is (= "foobarfoo" (clojure.string/replace-first "foobarfoo" #"baz" "bar")))
  (is (= "f$od" (clojure.string/replace-first "food" #"o" (clojure.string/re-quote-replacement "$"))))
  (is (= "f\\od" (clojure.string/replace-first "food" #"o" (clojure.string/re-quote-replacement "\\"))))
  (is (= "FOObarfoo" (clojure.string/replace-first "foobarfoo" #"foo" clojure.string/upper-case)))
  (is (= "foobarfoo" (clojure.string/replace-first "foobarfoo" #"baz" clojure.string/upper-case)))
  (is (= "OObarfoo" (clojure.string/replace-first "foobarfoo" #"f(o+)" (fn [[m g1]] (clojure.string/upper-case g1)))))
  (is (= "baz\\bangslash" (clojure.string/replace-first "bazslashbangslash" #"slash" (constantly "\\")))))

(deftest t-join
  (are [x coll] (= x (clojure.string/join coll))
    "" nil
    "" []
    "1" [1]
    "12" [1 2])
  (are [x sep coll] (= x (clojure.string/join sep coll))
    "1,2,3" \, [1 2 3]
    "" \, []
    "1" \, [1]
    "1 and-a 2 and-a 3" " and-a " [1 2 3]))

(deftest t-trim-newline
  (is (= "foo" (clojure.string/trim-newline "foo\n")))
  (is (= "foo" (clojure.string/trim-newline "foo\r\n")))
  (is (= "foo" (clojure.string/trim-newline "foo")))
  (is (= "" (clojure.string/trim-newline ""))))

(deftest t-capitalize
  (is (= "Foobar" (clojure.string/capitalize "foobar")))
  (is (= "Foobar" (clojure.string/capitalize "FOOBAR"))))

(deftest t-triml
  (is (= "foo " (clojure.string/triml " foo ")))
  (is (= "" (clojure.string/triml "   ")))
  (is (= "bar" (clojure.string/triml "\u2002 \tbar"))))

(deftest t-trimr
  (is (= " foo" (clojure.string/trimr " foo ")))
  (is (= "" (clojure.string/trimr "   ")))
  (is (= "bar" (clojure.string/trimr "bar\t \u2002"))))

(deftest t-trim
  (is (= "foo" (clojure.string/trim "  foo  \r\n")))
  (is (= "bar" (clojure.string/trim "\u2000bar\t \u2002"))))

(deftest t-upper-case
  (is (= "FOOBAR" (clojure.string/upper-case "Foobar"))))

(deftest t-lower-case
  (is (= "foobar" (clojure.string/lower-case "FooBar"))))

(deftest t-escape
  (is (= "&lt;foo&amp;bar&gt;"
         (clojure.string/escape "<foo&bar>" {\& "&amp;" \< "&lt;" \> "&gt;"})))
  (is (= " \\\"foo\\\" "
         (clojure.string/escape " \"foo\" " {\" "\\\""})))
  (is (= "faabor"
         (clojure.string/escape "foobar" {\a \o, \o \a}))))

(deftest t-blank
  (is (clojure.string/blank? nil))
  (is (clojure.string/blank? ""))
  (is (clojure.string/blank? " "))
  (is (clojure.string/blank? " \t \n \r "))
  (is (not (clojure.string/blank? " foo "))))

(deftest t-split-lines
  (is (= ["one" "two" "three"] (clojure.string/split-lines "one\ntwo\r\nthree")))
  (is (vector? (clojure.string/split-lines "one\ntwo\r\nthree")))
  (is (= (list "foo") (clojure.string/split-lines "foo"))))

(deftest t-index-of
  (is (let [sb  "tacos"] (= 2  (clojure.string/index-of sb "c"))))
  (is (let [sb  "tacos"] (= 2  (clojure.string/index-of sb \c))))
  (is (let [sb  "tacos"] (= 1  (clojure.string/index-of sb "ac"))))
  (is (let [sb  "tacos"] (= 3  (clojure.string/index-of sb "o" 2))))
  (is (let [sb  "tacos"] (= 3  (clojure.string/index-of sb  \o  2))))
  (is (let [sb  "tacos"] (= nil (clojure.string/index-of sb "z"))))
  (is (let [sb  "tacos"] (= nil (clojure.string/index-of sb \z))))
  (is (let [sb  "tacos"] (= nil (clojure.string/index-of sb "z" 2))))
  (is (let [sb  "tacos"] (= nil (clojure.string/index-of sb \z  2)))))

(deftest t-last-index-of
  (is (let [sb "banana"] (= 4 (clojure.string/last-index-of sb "n"))))
  (is (let [sb "banana"] (= 4 (clojure.string/last-index-of sb \n))))
  (is (let [sb "banana"] (= 3 (clojure.string/last-index-of sb "an"))))
  (is (let [sb "banana"] (= 4 (clojure.string/last-index-of sb "n"))))
  (is (let [sb "banana"] (= 4 (clojure.string/last-index-of sb "n" 5))))
  (is (let [sb "banana"] (= 4 (clojure.string/last-index-of sb \n  5))))
  (is (let [sb "banana"] (= nil (clojure.string/last-index-of sb "z"))))
  (is (let [sb "banana"] (= nil (clojure.string/last-index-of sb "z" 1))))
  (is (let [sb "banana"] (= nil (clojure.string/last-index-of sb \z  1)))))

(deftest t-starts-with?
  (is (clojure.string/starts-with? "clojure west" "clojure"))       ;;; (StringBuffer. "clojure west")
  (is (not (clojure.string/starts-with? "conj" "clojure"))))        ;;; (StringBuffer. "conj")

(deftest t-ends-with?
  (is (clojure.string/ends-with? "Clojure West" "West"))             ;;; (StringBuffer. "Clojure West")
  (is (not (clojure.string/ends-with? "Conj" "West"))))            ;;; (StringBuffer. "Conj")

(deftest t-includes?
  (is (let [sb "Clojure Applied Book"] (clojure.string/includes? sb "Applied")))
  (is (let [sb "Clojure Applied Book"] (not (clojure.string/includes? sb "Living")))))

(deftest empty-collections
  (is (= "()" (str ())))
  (is (= "{}" (str {})))
  (is (= "[]" (str []))))
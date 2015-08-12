(ns magic.reference)

(defn regexps [] #"foo")

(defn ratios [] 1/2)

(defn division [] (/ 1 2))

(defn bigints [] 1N)

(defn chars [] \a)

(defn vectors [] [1 (+ 1 2) 3])

(defn lists-1 [] '(1 2 3))
(defn lists-2 [] '(1 "2" 3))
(defn lists-3 [] '(1 (+ 1 2) 3))

(defn symbols-1 [] 'foo)
(defn symbols-1 [] 'foo/bar)

(defn types-1 [] clojure.lang.Symbol)
(defn types-2 [] System.Text.RegularExpressions.Regex)
(defn types-3 [] System.Int32)

(defn set!-var [] (set! *warn-on-reflection* true))

(def atom-vector (atom []))

(defn set!-var [] (set! *warn-on-reflection* true))
(defn set!-atom [] (set! atom-vector [1 2 3]))
(defn set!-vector3 []
  (let [^UnityEngine.Vector3 v (UnityEngine.Vector3. 1 2 3)]
    (set! (.x v) 10)))

(defn new-expr-1 [] (System.Drawing.Point.))
(defn new-expr-2 [] (System.Drawing.Point. 1 3))
(defn new-expr-3 [] (System.Drawing.Point. 1.2 3.1))
(defn new-expr-4 [] (System.Drawing.Point. 1.2 3))
(defn new-expr-5 [] (System.Drawing.Point. (System.Drawing.Size. 5 6)))
(defn new-expr-6 [] (System.Drawing.Point. (System.Drawing.Size. 5.2 6)))
(defn new-expr-7 [] (System.Drawing.Point. (System.Drawing.Size. 5.2 6.8)))

(defn instance-prop [] (.X (System.Drawing.Point. 5 6)))

(defn let-expr-1 [] (let [a "a"] a))
(defn let-expr-2 [] (let [a 1 b 2] (+ a b)))
(defn let-expr-3 [] (let [a 1 b 2] (let [b 9] (+ a b))))

(defn complex-let []
  (map inc (let [z 90
                 a (let [x 30
                         y "LISP"]
                     (+ z x (long (count y))))]
             (range a (let [b (long (count "Hello"))]
                        (+ a b))))))

(defn case-test [a]
  (case a
    1 "foo"
    2 "bar"
    :pant :never
    "else"))
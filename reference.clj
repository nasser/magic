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

(defn letfn-test [z]
  (letfn [(a [x] (b x)) (b [y] (+ y 1))]
    (a z)
    ))

(defn cast-test-1 [x]
  (UnityEngine.Mathf/Sin x))

(defn cast-test-2 [^long x]
  (UnityEngine.Mathf/Sin x))

(defn cast-test-3 [^double x]
  (UnityEngine.Mathf/Sin x))

(defn reflective [x]
  (.FlimFlam x))

(defn bad-types [a b c]
  (Vector3/Distance Vector3/one
                    (.AddForce (UnityEngine.Rigidbody.) a b c)))

(defn interopy [a b]
  (let [dist (Vector3/Distance a b)]
    (.AddForce (UnityEngine.Rigidbody.) dist dist dist)))

(defn get-literal-static-prop []
  Int32/MaxValue)

(defn get-nonliteral-static-prop []
  TimeSpan/Zero)

(defn non-overflow-add []
  (+ Int32/MaxValue 1))

(defn try-test []
  (try
    (+ 4 8)
    (catch Exception e (str "got" e))
    ; (finally (Console/WriteLine "OK"))
    ))

(defn keyword-get [a]
  (:foo a))

(defn ifaa [a] (if a a))

(defn nested-loops []
  (loop [a 2]
    (loop [b 0]
      (Console/WriteLine (str a b))
      (if (< b 100) (recur (inc b))))
    (if (< a 100) (recur (+ a 2)))))

(defn for-loop []
  (for [x (range 19)] x))

(defn infinite-loop []
  (loop [a 0] (recur (inc a))))

(defn vector-loop [^UnityEngine.Vector3 x]
  (loop [a x]
    (if (< (Vector3/Distance a Vector3/zero) 100)
      (recur (UnityEngine.Vector3/op_Addition a Vector3/right))
      a)))

(deftype AType [a b c])

(defn kw-invoke [a]
  (:foo a))

(defn empty-vec []
  [])

(defn small-vec []
  (dotimes [_ 10]
    (time
      (dotimes [_ 1e7]
        [:this 0
         :is 1 
         :a 2
         :small 3
         :map 4]))))

(defn nested-fns [a]
  (fn [b] (+ a b)))
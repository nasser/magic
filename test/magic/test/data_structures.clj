(ns magic.test.data-structures
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

(deftest test-equality
  ; nil is not equal to any other value
  (are [x] (not (= nil x))
    true false
    0 0.0
    \space
    "" #""
    () [] #{} {}
    (lazy-seq nil)  ; SVN 1292: fixed (= (lazy-seq nil) nil)
    (lazy-seq ())
    (lazy-seq [])
    (lazy-seq {})
    (lazy-seq #{})
    (lazy-seq "")
    (lazy-seq (into-array []))
    (new Object))

  ; numbers equality across types (see tests below - NOT IMPLEMENTED YET)
  
  ; ratios
  (is (== 1/2 0.5))
  (is (== 1/1000 0.001))
  (is (not= 2/3 0.6666666666666666))

  ; vectors equal other seqs by items equality
  (are [x y] (= x y)
    '() []        ; regression fixed in r1208; was not equal
    '(1) [1]
    '(1 2) [1 2]

    [] '()        ; same again, but vectors first
    [1] '(1)
    [1 2] '(1 2))
  (is (not= [1 2] '(2 1)))  ; order of items matters
  
  ; list and vector vs. set and map
  (are [x y] (not= x y)
      ; only () equals []
    () #{}
    () {}
    [] #{}
    [] {}
    #{} {}
      ; only '(1) equals [1]
    '(1) #{1}
    [1] #{1})

  ; sorted-map, hash-map and array-map - classes differ, but content is equal
  
;; TODO: reimplement all-are with new do-template?  
;;   (all-are (not= (class _1) (class _2))
;;       (sorted-map :a 1)
;;       (hash-map   :a 1)
;;       (array-map  :a 1))
;;   (all-are (= _1 _2)
;;       (sorted-map)
;;       (hash-map)
;;       (array-map))
;;   (all-are (= _1 _2)
;;       (sorted-map :a 1)
;;       (hash-map   :a 1)
;;       (array-map  :a 1))
;;   (all-are (= _1 _2)
;;       (sorted-map :a 1 :z 3 :c 2)
;;       (hash-map   :a 1 :z 3 :c 2)
;;       (array-map  :a 1 :z 3 :c 2))
  
  ; struct-map vs. sorted-map, hash-map and array-map
 (is (not= (sorted-set :a) (sorted-set 1)))

  ; sorted-set vs. hash-set
  (is (not= (class (sorted-set 1)) (class (hash-set 1))))
  (are [x y] (= x y)
    (sorted-set-by <) (hash-set)
    (sorted-set-by < 1) (hash-set 1)
    (sorted-set-by < 3 2 1) (hash-set 3 2 1)
    (sorted-set) (hash-set)
    (sorted-set 1) (hash-set 1)
    (sorted-set 3 2 1) (hash-set 3 2 1)))

(deftest test-count
  (are [x y] (= (count x) y)
    clojure.lang.PersistentQueue/EMPTY 0
    (into clojure.lang.PersistentQueue/EMPTY [:a :b]) 2
    (-> (into clojure.lang.PersistentQueue/EMPTY [:a :b]) pop pop) 0

    nil 0

    () 0
    '(1) 1
    '(1 2 3) 3

    [] 0
    [1] 1
    [1 2 3] 3

    #{} 0
    #{1} 1
    #{1 2 3} 3

    {} 0
    {:a 1} 1
    {:a 1 :b 2 :c 3} 3

    "" 0
    "a" 1
    "abc" 3

    (into-array []) 0
    (into-array [1]) 1
    (into-array [1 2 3]) 3

    (System.Collections.ArrayList. []) 0                   ;;; java.util.ArrayList.
    (System.Collections.ArrayList. [1]) 1                  ;;; java.util.ArrayList.
    (System.Collections.ArrayList. [1 2 3]) 3              ;;; java.util.ArrayList.
    
    (System.Collections.Hashtable. {}) 0                     ;;; java.util.HashMap.
    (System.Collections.Hashtable. {:a 1}) 1                 ;;; java.util.HashMap.
    (System.Collections.Hashtable. {:a 1 :b 2 :c 3}) 3)    ;;; java.util.HashMap.
  
  ; different types
  (are [x]  (= (count [x]) 1)
    nil true false
    0 0.0 "" \space
    () [] #{} {}))


(deftest test-conj
  ; doesn't work on strings or arrays

  (are [x y] (= x y)
    (conj nil 1) '(1)
    (conj nil 3 2 1) '(1 2 3)

    (conj nil nil) '(nil)
    (conj nil nil nil) '(nil nil)
    (conj nil nil nil 1) '(1 nil nil)

      ; list -> conj puts the item at the front of the list
    (conj () 1) '(1)
    (conj () 1 2) '(2 1)

    (conj '(2 3) 1) '(1 2 3)
    (conj '(2 3) 1 4 3) '(3 4 1 2 3)

    (conj () nil) '(nil)
    (conj () ()) '(())

      ; vector -> conj puts the item at the end of the vector
    (conj [] 1) [1]
    (conj [] 1 2) [1 2]

    (conj [2 3] 1) [2 3 1]
    (conj [2 3] 1 4 3) [2 3 1 4 3]

    (conj [] nil) [nil]
    (conj [] []) [[]]

      ; map -> conj expects another (possibly single entry) map as the item,
      ;   and returns a new map which is the old map plus the entries
      ;   from the new, which may overwrite entries of the old.
      ;   conj also accepts a MapEntry or a vector of two items (key and value).
    (conj {} {}) {}
    (conj {} {:a 1}) {:a 1}
    (conj {} {:a 1 :b 2}) {:a 1 :b 2}
    (conj {} {:a 1 :b 2} {:c 3}) {:a 1 :b 2 :c 3}
    (conj {} {:a 1 :b 2} {:a 3 :c 4}) {:a 3 :b 2 :c 4}

    (conj {:a 1} {:a 7}) {:a 7}
    (conj {:a 1} {:b 2}) {:a 1 :b 2}
    (conj {:a 1} {:a 7 :b 2}) {:a 7 :b 2}
    (conj {:a 1} {:a 7 :b 2} {:c 3}) {:a 7 :b 2 :c 3}
    (conj {:a 1} {:a 7 :b 2} {:b 4 :c 5}) {:a 7 :b 4 :c 5}

    (conj {} (first {:a 1})) {:a 1}           ; MapEntry
    (conj {:a 1} (first {:b 2})) {:a 1 :b 2}
    (conj {:a 1} (first {:a 7})) {:a 7}
    (conj {:a 1} (first {:b 2}) (first {:a 5})) {:a 5 :b 2}

    (conj {} [:a 1]) {:a 1}                   ; vector
    (conj {:a 1} [:b 2]) {:a 1 :b 2}
    (conj {:a 1} [:a 7]) {:a 7}
    (conj {:a 1} [:b 2] [:a 5]) {:a 5 :b 2}

    (conj {} {nil {}}) {nil {}}
    (conj {} {{} nil}) {{} nil}
    (conj {} {{} {}}) {{} {}}

      ; set
    (conj #{} 1) #{1}
    (conj #{} 1 2 3) #{1 2 3}

    (conj #{2 3} 1) #{3 1 2}
    (conj #{3 2} 1) #{1 2 3}

    (conj #{2 3} 2) #{2 3}
    (conj #{2 3} 2 3) #{2 3}
    (conj #{2 3} 4 1 2 3) #{1 2 3 4}

    (conj #{} nil) #{nil}
    (conj #{} #{}) #{#{}}))


;; *** Lists and Vectors ***

(deftest test-peek
  ; doesn't work for sets and maps

  (are [x y] (= x y)
    (peek nil) nil

      ; list = first
    (peek ()) nil
    (peek '(1)) 1
    (peek '(1 2 3)) 1

    (peek '(nil)) nil     ; special cases
    (peek '(1 nil)) 1
    (peek '(nil 2)) nil
    (peek '(())) ()
    (peek '(() nil)) ()
    (peek '(() 2 nil)) ()

      ; vector = last
    (peek []) nil
    (peek [1]) 1
    (peek [1 2 3]) 3

    (peek [nil]) nil      ; special cases
    (peek [1 nil]) nil
    (peek [nil 2]) 2
    (peek [[]]) []
    (peek [[] nil]) nil
    (peek [[] 2 nil]) nil))


(deftest test-pop
  ; doesn't work for sets and maps

  ; collection cannot be empty

  (are [x y] (= x y)
    (pop nil) nil

      ; list - pop first
    (pop '(1)) ()
    (pop '(1 2 3)) '(2 3)

    (pop '(nil)) ()
    (pop '(1 nil)) '(nil)
    (pop '(nil 2)) '(2)
    (pop '(())) ()
    (pop '(() nil)) '(nil)
    (pop '(() 2 nil)) '(2 nil)

      ; vector - pop last
    (pop [1]) []
    (pop [1 2 3]) [1 2]

    (pop [nil]) []
    (pop [1 nil]) [1]
    (pop [nil 2]) [nil]
    (pop [[]]) []
    (pop [[] nil]) [[]]
    (pop [[] 2 nil]) [[] 2]))


;; *** Lists (IPersistentList) ***

(deftest test-list
  (are [x]  (list? x)
    ()
    '()
    (list)
    (list 1 2 3))

  ; order is important
  (are [x y] (not (= x y))
    (list 1 2) (list 2 1)
    (list 3 1 2) (list 1 2 3))

  (are [x y] (= x y)
    '() ()
    (list) '()
    (list 1) '(1)
    (list 1 2) '(1 2)

      ; nesting
    (list 1 (list 2 3) (list 3 (list 4 5 (list 6 (list 7)))))
    '(1 (2 3) (3 (4 5 (6 (7)))))

      ; different data structures
    (list true false nil)
    '(true false nil)
    (list 1 2.5 2/3 "ab" \x 'cd :kw)
    '((1 2) [3 4] {:a 1 :b 2} #{:c :d})

      ; evaluation
    (list (+ 1 2) [(+ 2 3) 'a] (list (* 2 3) 8))
    '(3 [5 a] (6 8))

      ; special cases
    (list nil) '(nil)
    (list 1 nil) '(1 nil)
    (list nil 2) '(nil 2)
    (list ()) '(())
    (list 1 ()) '(1 ())
    (list () 2) '(() 2)))


;; *** Maps (IPersistentMap) ***

(deftest test-find
  (are [x y] (= x y)
    (find {} :a) nil

    (find {:a 1} :a) [:a 1]
    (find {:a 1} :b) nil
    (find {nil 1} nil) [nil 1]

    (find {:a 1 :b 2} :a) [:a 1]
    (find {:a 1 :b 2} :b) [:b 2]
    (find {:a 1 :b 2} :c) nil

    (find {} nil) nil
    (find {:a 1} nil) nil
    (find {:a 1 :b 2} nil) nil))

(deftest test-contains?
  (are [x y] (= x y)
    (contains? {} :a) false
    (contains? {} nil) false

    (contains? {:a 1} :a) true
    (contains? {:a 1} :b) false
    (contains? {:a 1} nil) false
    (contains? {nil 1} nil) true

    (contains? {:a 1 :b 2} :a) true
    (contains? {:a 1 :b 2} :b) true
    (contains? {:a 1 :b 2} :c) false
    (contains? {:a 1 :b 2} nil) false

      ; sets
    (contains? #{} 1) false
    (contains? #{} nil) false

    (contains? #{1} 1) true
    (contains? #{1} 2) false
    (contains? #{1} nil) false

    (contains? #{1 2 3} 1) true
    (contains? #{1 2 3} 3) true
    (contains? #{1 2 3} 10) false
    (contains? #{1 2 3} nil) false)
  
  (are [x y] (= x y)
    (contains? [] 0) false
    (contains? [] -1) false
    (contains? [] 1) false

    (contains? [1] 0) true
    (contains? [1] -1) false
    (contains? [1] 1) false

    (contains? [1 2 3] 0) true
    (contains? [1 2 3] 2) true
    (contains? [1 2 3] 3) false
    (contains? [1 2 3] -1) false

      ; arrays
    (contains? (into-array []) 0) false
    (contains? (into-array []) -1) false
    (contains? (into-array []) 1) false

    (contains? (into-array [1]) 0) true
    (contains? (into-array [1]) -1) false
    (contains? (into-array [1]) 1) false

    (contains? (into-array [1 2 3]) 0) true
    (contains? (into-array [1 2 3]) 2) true
    (contains? (into-array [1 2 3]) 3) false
    (contains? (into-array [1 2 3]) -1) false))

(deftest test-keys
  (are [x y] (= x y)      ; other than map data structures
    (keys ()) nil
    (keys []) nil
    (keys #{}) nil
    (keys "") nil)

  (are [x y] (= x y)
      ; (class {:a 1}) => clojure.lang.PersistentArrayMap
    (keys {}) nil
    (keys {:a 1}) '(:a)
    (keys {nil 1}) '(nil)

      ; (class (sorted-map :a 1)) => clojure.lang.PersistentTreeMap
    (keys (sorted-map)) nil
    (keys (sorted-map :a 1)) '(:a)

      ; (class (hash-map :a 1)) => clojure.lang.PersistentHashMap
    (keys (hash-map)) nil
    (keys (hash-map :a 1)) '(:a))

  (is (= {:hi :there} 
         (let [m {:a 1 :b 2}
               k (keys m)] (meta (with-meta k {:hi :there}))))))

(deftest test-vals
  (are [x y] (= x y)      ; other than map data structures
    (vals ()) nil
    (vals []) nil
    (vals #{}) nil
    (vals "") nil)

  (are [x y] (= x y)
      ; (class {:a 1}) => clojure.lang.PersistentArrayMap
    (vals {}) nil
    (vals {:a 1}) '(1)
    (vals {nil 1}) '(1)
    
      ; (class (sorted-map :a 1)) => clojure.lang.PersistentTreeMap
    (vals (sorted-map)) nil
    (vals (sorted-map :a 1)) '(1)
    
      ; (class (hash-map :a 1)) => clojure.lang.PersistentHashMap
    (vals (hash-map)) nil
    (vals (hash-map :a 1)) '(1))

  (is (= {:hi :there} 
         (let [m {:a 1 :b 2}
               v (vals m)] (meta (with-meta v {:hi :there}))))))

(deftest test-key
  (are [x]  (= (key (first (hash-map x :value))) x)
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


(deftest test-val
  (are [x]  (= (val (first (hash-map :key x))) x)
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

(deftest test-get
  (are [x y] (let [m {:a 1, :b 2, :c {:d 3, :e 4}, :f nil, :g false, nil {:h 5}}]
               (= x y))
    (get m :a) 1
    (get m :e) nil
    (get m :e 0) 0
    (get m nil) {:h 5}
    (get m :b 0) 2
    (get m :f 0) nil

    (get-in m [:c :e]) 4
    (get-in m '(:c :e)) 4
    (get-in m [:c :x]) nil
    (get-in m [:f]) nil
    (get-in m [:g]) false
    (get-in m [:h]) nil
    (get-in m []) m
    (get-in m nil) m

    (get-in m [:c :e] 0) 4
    (get-in m '(:c :e) 0) 4
    (get-in m [:c :x] 0) 0
    (get-in m [:b] 0) 2
    (get-in m [:f] 0) nil
    (get-in m [:g] 0) false
    (get-in m [:h] 0) 0
    (get-in m [:x :y] {:y 1}) {:y 1}
    (get-in m [] 0) m
    (get-in m nil 0) m))

(deftest test-nested-map-destructuring
  (are [i o]
       (let [sample-map {:a 1 :b {:a 2}}
             {ao1 :a {ai1 :a} :b} sample-map
             {ao2 :a {ai2 :a :as m1} :b :as m2} sample-map
             {ao3 :a {ai3 :a :as m} :b :as m} sample-map
             {{ai4 :a :as m} :b ao4 :a :as m} sample-map]
        (and (= i 2)
             (= o 1)))
    ai1 ao1
    ai2 ao2
    ai3 ao3
    ai4 ao4))

(deftest test-hash-set
  (are [x] (set? x)
    #{}
    #{1 2}
    (hash-set)
    (hash-set 1 2))

  ; order isn't important
  (are [x y] (= x y)
    #{1 2} #{2 1}
    #{3 1 2} #{1 2 3}
    (hash-set 1 2) (hash-set 2 1)
    (hash-set 3 1 2) (hash-set 1 2 3))


  (are [x y] (= x y)
      ; equal classes
    (class #{}) (class (hash-set))
    (class #{1 2}) (class (hash-set 1 2))

      ; creating
    (hash-set) #{}
    (hash-set 1) #{1}
    (hash-set 1 2) #{1 2}

      ; nesting
    (hash-set 1 (hash-set 2 3) (hash-set 3 (hash-set 4 5 (hash-set 6 (hash-set 7)))))
    #{1 #{2 3} #{3 #{4 5 #{6 #{7}}}}}

      ; different data structures
    (hash-set true false nil)
    #{true false nil}
    (hash-set 1 2.5 2/3 "ab" \x 'cd :kw)
    #{1 2.5 2/3 "ab" \x 'cd :kw}
    (hash-set (list 1 2) [3 4] {:a 1 :b 2} #{:c :d})
    #{'(1 2) [3 4] {:a 1 :b 2} #{:c :d}}

      ; evaluation
    (hash-set (+ 1 2) [(+ 2 3) :a] (hash-set (* 2 3) 8))
    #{3 [5 :a] #{6 8}}

      ; special cases
    (hash-set nil) #{nil}
    (hash-set 1 nil) #{1 nil}
    (hash-set nil 2) #{nil 2}
    (hash-set #{}) #{#{}}
    (hash-set 1 #{}) #{1 #{}}
    (hash-set #{} 2) #{#{} 2}
    #_(hash-set (Integer. -1)) #_(hash-set (Long. -1))))

(deftest test-sorted-set
  ; creates set?
  (are [x] (set? x)
    (sorted-set)
    (sorted-set 1 2))

  ; equal and unique
  (are [x] (and (= (sorted-set x) #{x})
                (= (sorted-set x x) (sorted-set x)))
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
    ()  ; '(1 2)
    [] [1 2]
    {}  ; {:a 1 :b 2}
    #{} ; #{1 2}
    )
  (are [x y] (= x y)
      ; generating
    (sorted-set) #{}
    (sorted-set 1) #{1}
    (sorted-set 1 2) #{1 2}

      ; sorting
    (seq (sorted-set 5 4 3 2 1)) '(1 2 3 4 5)

      ; special cases
    (sorted-set nil) #{nil}
    (sorted-set 1 nil) #{nil 1}
    (sorted-set nil 2) #{nil 2}
    (sorted-set #{}) #{#{}}))

(deftest test-sorted-set-by
  ; creates set?
  (are [x] (set? x)
    (sorted-set-by <)
    (sorted-set-by < 1 2))

  ; equal and unique
  (are [x] (and (= (sorted-set-by compare x) #{x})
                (= (sorted-set-by compare x x) (sorted-set-by compare x)))
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
    ()  ; '(1 2)
    [] [1 2]
    {}  ; {:a 1 :b 2}
    #{} ; #{1 2}
    )

  (are [x y] (= x y)
      ; generating
    (sorted-set-by >) #{}
    (sorted-set-by > 1) #{1}
    (sorted-set-by > 1 2) #{1 2}

      ; sorting
    (seq (sorted-set-by < 5 4 3 2 1)) '(1 2 3 4 5)

      ; special cases
    (sorted-set-by compare nil) #{nil}
    (sorted-set-by compare 1 nil) #{nil 1}
    (sorted-set-by compare nil 2) #{nil 2}
    (sorted-set-by compare #{}) #{#{}}))

(deftest test-set
  ; set?
  (are [x] (set? (set x))
    () '(1 2)
    [] [1 2]
    #{} #{1 2}
    {} {:a 1 :b 2}
    (into-array []) (into-array [1 2])
    "" "abc")

  ; unique
  (are [x] (= (set [x x]) #{x})
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
    #{} #{1 2})

  ; conversion
  (are [x y] (= (set x) y)
    () #{}
    '(1 2) #{1 2}

    [] #{}
    [1 2] #{1 2}

    #{} #{}         ; identity
    #{1 2} #{1 2}   ; identity

    {} #{}
    {:a 1 :b 2} #{[:a 1] [:b 2]}

    (into-array []) #{}
    (into-array [1 2]) #{1 2}

    "" #{}
    "abc" #{\a \b \c}))

(deftest test-disj
  ; identity
  (are [x] (= (disj x) x)
    nil
    #{}
    #{1 2 3}
      ; different data types
    #{nil
      false true
      0 42
      0.0 3.14
      2/3
      0M 1M
      \c
      "" "abc"
      'sym
      :kw
      [] [1 2]
      {} {:a 1 :b 2}
      #{} #{1 2}})

  ; type identity
  (are [x] (= (class (disj x)) (class x))
    (hash-set)
    (hash-set 1 2)
    (sorted-set)
    (sorted-set 1 2))

  (are [x y] (= x y)
    (disj nil :a) nil
    (disj nil :a :b) nil

    (disj #{} :a) #{}
    (disj #{} :a :b) #{}

    (disj #{:a} :a) #{}
    (disj #{:a} :a :b) #{}
    (disj #{:a} :c) #{:a}

    (disj #{:a :b :c :d} :a) #{:b :c :d}
    (disj #{:a :b :c :d} :a :d) #{:b :c}
    (disj #{:a :b :c :d} :a :b :c) #{:d}
    (disj #{:a :b :c :d} :d :a :c :b) #{}

    (disj #{nil} :a) #{nil}
    (disj #{nil} #{}) #{nil}
    (disj #{nil} nil) #{}

    (disj #{#{}} nil) #{#{}}
    (disj #{#{}} #{}) #{}
    (disj #{#{nil}} #{nil}) #{}))
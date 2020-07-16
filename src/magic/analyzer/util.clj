(ns magic.analyzer.util
  (:require [clojure.string :as string]))

;; TODO replace with error multimethod
(defmacro throw! [& e]
  `(throw (ex-info (str ~@e) {})))

(defn var-reference
  "Get the var reference associated with an AST, or nil"
  [{:keys [init var fn] :as ast}]
  (cond
    ;; an actual var
    (var? ast) ast
    ;; invoke
    fn (:var fn)
    ;; var literal
    var var 
    ;; local
    init (:var init)))

(defn var-interfaces
  "The interfaces the var associated with the AST implements, or []"
  [ast]
  (if-let [v (var-reference ast)]
    (if (deref v)
      (-> v deref type .GetInterfaces)
      [])
    []))

(defn var-type
  "The type of the instance the var is associated with"
  [ast]
  (if-let [v (var-reference ast)]
    (-> v deref type)))

(defn var-fixed-arities
  "Fixed arities a var supports"
  [ast]
  (->> ast
       var-reference
       meta
       :arglists
       (map (fn [args] (take-while #(not= % '&) args)))
       (map count)
       set))

(defn var-symbol [var]
  (when (var? var) (symbol (str (.Namespace var)) (str (.Symbol var)))))

(defn var-variadic-arity
  "Size of the variadic arity or nil if none exists"
  [ast]
  (let [variadic-arglists
        (->> ast
             var-reference
             meta
             :arglists
             (filter (fn [args] (some #(= % '&) args))))]
    (if (= (count variadic-arglists) 1) 
      (->> variadic-arglists
           first
           (take-while #(not= % '&))
           count
           inc)
      nil)))

(defn seq-sentence
  ([s] (seq-sentence s ", " " or "))
  ([s joiner last-joiner]
   (cond 
     (= (count s) 0)
     "" ;; ???
     (= (count s) 1)
     (str (first s))
     :else
     (let [butlast-seq (butlast s)
         last-seq (last s)]
     (str (string/join joiner butlast-seq) last-joiner last-seq)))))

(defn jagged-array-type [t rank]
  (if (pos? rank)
    (recur (.MakeArrayType t) (dec rank))
    t))

(defn extract-jagged-array-type [t depth]
  (if (pos? depth)
    (recur (.GetElementType  t) (dec depth))
    t))

(defn value [ast]
  (or (:val ast)
      (-> ast :init :val)))
;; this should be clojure.tools.analyzer.clr
(ns magic.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :refer [postwalk]]
            [clojure.tools.analyzer.env :refer [with-env]]
            [clojure.tools.analyzer.passes.elide-meta :refer [elides elide-meta]]
            [clojure.test :refer [deftest is]]
            [clojure.tools.analyzer.utils :refer [resolve-sym]]))

(defn desugar-host-expr [[op & expr :as form]]
  (if (symbol? op)
    (let [opname (name op)]
      (cond

       (= (first opname) \.) ; (.foo bar ..)
       (let [[target & args] expr
             args (list* (symbol (subs opname 1)) args)]
         (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) ia
                                      (first args) args)) ;; a method call or a field access
           (meta form)))

       (= (last opname) \.) ;; (class. ..)
       (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
         (meta form))

       :else form))
    form))

(defn macroexpand-1 [form env]
  (if (seq? form)
    (let [op (first form)]
      (if (ana/specials op)
        form
        (let [v (resolve-sym op env)]
          (if (and (not (-> env :locals (get op))) ;; locals cannot be macros
                   (:macro (meta v)))
            (apply v form env (rest form)) ; (m &form &env & args)
            (desugar-host-expr form)))))
        form))

(def e {:context    :ctx/expr
        :locals     {}
        :ns         'user})

(def e1 (atom {:namespaces {'user         {:mappings (ns-map 'clojure.core)
                                           :aliases  {}
                                           :ns       'user}
                            'clojure.core {:mappings (ns-map 'clojure.core)
                                           :aliases {}
                                           :ns      'clojure.core}}}))
(defn ast [form]
  (binding [ana/macroexpand-1 macroexpand-1
            ana/create-var    (fn [sym env]
                                (doto (intern (:ns env) sym)
                                  (reset-meta! (meta sym))))
            ana/parse         ana/-parse
            ana/var?          var?
            elides            {:all #{:line :column :file :source-span}}]
    (with-env e1
              (postwalk (ana/analyze form e) elide-meta))))
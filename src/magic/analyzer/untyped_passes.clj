(ns magic.analyzer.untyped-passes
  (:require 
   [clojure.tools.analyzer.passes :refer [schedule]]
   [clojure.tools.analyzer.ast :refer [nodes update-children]]
   [clojure.tools.analyzer.passes
    [trim :refer [trim]]
    [collect-closed-overs :refer [collect-closed-overs]]
    [uniquify :refer [uniquify-locals]]]
   [magic.analyzer
    [remove-local-children :refer [remove-local-children]]]))

(defn collect-vars
  "Collect all vars within :fn nodes into :vars key"
  [ast]
  (if (= :fn (:op ast))
    (assoc ast :vars (->> ast nodes (filter #(= :var (:op %)))))
    ast))

(def ^:dynamic *stack-empty?* true)

(defn compute-empty-stack-context
  "Stores :empty-stack? into environments reflecting expressions where
   the CLR evaluation stack is expected to be empty"
  {:pass-info {:walk :none}}
  [{:keys [op] {:keys [context]} :env :as ast}]
  (binding [*stack-empty?*
            (or (and *stack-empty?* (not= context :ctx/expr))
                (= op :fn-method))]
    (-> ast
        (assoc-in [:env :empty-stack?] *stack-empty?*)
        (update-children compute-empty-stack-context))))

(defn remove-empty-throw-children
  "Removes :exception from the :children vector of :throw nodes when
   :exception is nil"
  {:pass-info {:walk :any :before #{#'compute-empty-stack-context}}}
  [{:keys [op exception] :as ast}]
  (case op
    :throw
    (if-not exception
      (dissoc ast :children :exception)
      ast)
    ast))

(defn propagate-defn-name
  {:pass-info {:walk :any}}
  [{:keys [op name init] :as ast}]
  (case op
    :def
    (case (:op init)
      :fn
      (assoc-in ast [:init :name] name)
      :with-meta
      (assoc-in ast [:init :expr :name] name)
      ast)
    ast))

(defn extract-form-meta
  {:pass-info {:walk :any}}
  [{:keys [op form] :as ast}]
  (case op
    (:binding) ;; TODO just bindings now to fix issue with parameters
    (assoc ast :meta (meta form))
    ast))

(defn compute-outside-type
  {:pass-info {:walk :none}}
  [{:keys [op] :as ast}]
  (case op
    (:fn :proxy :reify)
    ast
    #_else
    (-> ast
        (assoc :outside-type? true)
        (update-children compute-outside-type))))

(defn track-constant-literals
  {:pass-info {:walk :post}}
  [{:keys [op items keys vals] :as ast}]
  (case op
    (:quote :const)
    (assoc ast :constant? true)
    :map
    (assoc ast :constant? (and (every? :constant? keys)
                               (every? :constant? vals)))
    (:set :vector)
    (assoc ast :constant? (every? :constant? items))
    #_else ast))

(def untyped-pass-set
#{#'collect-vars
  #'track-constant-literals
  #'propagate-defn-name
  #'compute-outside-type
  #'extract-form-meta
  #'remove-local-children
  #'collect-closed-overs
  #'trim
  #'uniquify-locals
  #'compute-empty-stack-context
  #'remove-empty-throw-children})

(def scheduled-passes
  (schedule untyped-pass-set))

(defn untyped-passes [ast]
  (scheduled-passes ast))
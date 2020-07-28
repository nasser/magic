(ns magic.analyzer.untyped-passes
  (:require 
   [clojure.tools.analyzer.passes :refer [schedule]]
   [clojure.tools.analyzer.ast :refer [nodes children update-children]]
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

(defn collect-keywords
  "Collect all keywords within :fn nodes into :keywords key"
  [ast]
  (if (= :fn (:op ast))
    (assoc ast :keywords (->> ast nodes (filter #(and (= :const (:op %))
                                                      (keyword? (:val %))))))
    ast))

(def ^:dynamic *stack-empty?* true)

(defn compute-empty-stack-context
  "Stores :empty-stack? into environments reflecting expressions where
   the CLR evaluation stack is expected to be empty"
  {:pass-info {:walk :none}}
  [{:keys [op] {:keys [context]} :env :as ast}]
  (binding [*stack-empty?*
            (or (and *stack-empty?* (not= context :ctx/expr))
                (= op :fn-method)
                (= op :proxy-method)
                (= op :reify-method)
                (= op :deftype-method))]
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

(defn update-context [ast ctx]
  (assoc-in
   (update-children ast #(update-context % ctx))
   [:env :context]
   ctx))

(defn throwing-do [exprs]
  (let [[exprs* throwing-exprs] (split-with (comp not :throws?) exprs)]
    (when-let [ret* (first throwing-exprs)]
      {:op :do
       :statements (mapv #(update-context % :ctx/statement) exprs*)
       :ret (update-context ret* :ctx/return)
       :throws? true
       :children [:statements :ret]})))

(defn treat-throw-as-return
  {:pass-info {:walk :post :before #{#'compute-empty-stack-context #'uniquify-locals #'compute-outside-type}}}
  [{:keys [op] :as ast}]
  (case op
    :throw
    (assoc ast :throws? true)
    :if
    (let [{:keys [test else then]} ast]
      (if (:throws? test)
        test
        (assoc ast :throws?
               (and (:throws? else)
                    (:throws? then)))))
    :do
    (let [{:keys [statements ret]} ast
          [statements* throwing-statements] (split-with (comp not :throws?) statements)]
      (if-let [ret* (first throwing-statements)]
        (assoc ast
               :ret ret*
               :statements (vec statements*)
               :throws? true)
        (assoc ast :throws? (:throws? ret))))
    (:let :loop)
    (let [{:keys [bindings body]} ast
          [bindings* throwing-bindings] (split-with (comp not :throws? :init) bindings)]
      (if-let [body* (first throwing-bindings)]
        (assoc ast
               :body (:init body*)
               :bindings (vec bindings*)
               :throws? true)
        (assoc ast :throws? (:throws? body))))
    :case
    (let [{:keys [default expressions]} ast]
      (assoc ast :throws?
             (and (:throws? default)
                  (every? :throws? expressions))))
    :catch
    (assoc ast :throws? (-> ast :body :throws?))
    :try
    (let [throws? (and (-> ast :body :throws?)
                       (every? :throws? (-> ast :catches)))
          throws? (if-let [finally-expr (-> ast :finally)]
                    (or throws? (:throws? finally-expr))
                    throws?)]
      (assoc ast :throws? throws?))
    :invoke
    (merge ast 
           (throwing-do (:args ast)))
    (:def
     :fn :fn-method
     :proxy :proxy-method
     :reify :reify-method
     :deftype :deftype-method)
    ast
    #_else
    (if-let [ast* (throwing-do (children ast))]
      (merge ast ast*)
      ast)))

(defn wrap-tagged-expressions 
  {:pass-info {:walk :any :after #{#'uniquify-locals}}}
  [{:keys [op form] :as ast}]
  (case op
    :binding
    ast
    (if-let [tag (-> form meta :tag)]
      {:op :tagged
       :form (vary-meta form dissoc :tag)
       :tag tag
       :expr (update ast :form vary-meta dissoc :tag)
       :children [:expr]}
      ast)))

(def ^:dynamic *recur-allowed* true)

(defn prevent-recur-out-of-try
  {:pass-info {:walk :none}}
  [{:keys [op] :as ast}]
  (case op
    :recur
    (if-not *recur-allowed*
      (throw (ex-info "Cannot recur out of try" {}))
      (update-children ast prevent-recur-out-of-try))
    (:fn :loop)
    (binding [*recur-allowed* true]
      (update-children ast prevent-recur-out-of-try))
    :try
    (binding [*recur-allowed* false]
      (update-children ast prevent-recur-out-of-try))
    (update-children ast prevent-recur-out-of-try)))

(defn explicit-const-type
  {:pass-info {:walk :any}}
  [{:keys [op val] :as ast}]
  (case op
   :const
    (assoc ast :const-type (type val))
    ast))

(def ^:dynamic *fn-name* nil)

(defn propagate-fn-name
  {:pass-info {:walk :none}}
  [{:keys [op name local] :as ast}]
  (case op
    :fn
    (binding [*fn-name* (or name (:form local))]
      (update-children ast propagate-fn-name))
    #_else
    (-> ast
        (assoc :containing-fn-name *fn-name*)
        (update-children propagate-fn-name))))

(def untyped-pass-set
  #{#'collect-vars
    #'collect-keywords
    #'track-constant-literals
    #'propagate-defn-name
    #'compute-outside-type
    #'extract-form-meta
    #'remove-local-children
    #'collect-closed-overs
    #'trim
    #'uniquify-locals
    #'wrap-tagged-expressions
    #'compute-empty-stack-context
    #'remove-empty-throw-children
    #'treat-throw-as-return
    #'prevent-recur-out-of-try
    #'propagate-fn-name
    #'explicit-const-type})

(def scheduled-passes
  (schedule untyped-pass-set))

(defn untyped-passes [ast]
  (scheduled-passes ast))
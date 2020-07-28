(ns magic.analyzer.analyze-host-forms
  (:require
   [clojure.tools.analyzer.passes
    [uniquify :refer [uniquify-locals]]]
   [magic.interop :as interop]
   [magic.core :as magic]
   [magic.analyzer
    [errors :refer [error] :as errors]
    [binder :refer [select-method]]
    [types :refer [ast-type] :as types]]
   [magic.emission :refer [*module*]])
  (:import [System.Reflection BindingFlags]
           [System.Reflection.Emit TypeBuilder]))

(defn get-all-methods 
  ([t] (into #{}
             (concat
              (.GetMethods t)
              (mapcat get-all-methods (.GetInterfaces t))
              (when-let [base (.BaseType t)]
                (get-all-methods base)))))
  ([t name] (->> t get-all-methods (filter #(= name (.Name %))))))

(def public-instance (enum-or BindingFlags/Instance BindingFlags/Public))
(def public-static (enum-or BindingFlags/Static BindingFlags/Public))
(def public-instance-static (enum-or BindingFlags/Instance BindingFlags/Static BindingFlags/Public))

(defn ensure-class [c form]
  (or 
   (types/resolve c)
   (error
    ::errors/missing-type
    {:type c :form form})))

(defn analyze-enums
  [{:keys [op target field env] :as ast}]
  (if (and (= op :static-field)
           (-> target :val .IsEnum))
    {:op :const
     :type :enum
     :literal? true
     :val (Enum/ToObject (-> target :val) (.GetRawConstantValue field))
     :env env}
    ast))

(defn analyze-type
  "Analyze foo into a type"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [op children class] :as ast}]
  (if (= :maybe-class op)
    (let [target-type (ensure-class (name class) (:form ast))]
      (merge (dissoc ast :class)
             {:children (vec (remove #(= % :class) children))}
             {:op :const
              :type :class
              :literal? true
              :val target-type
              :form target-type}))
    ast))

(defn analyze-host-field
  "Analyze (.-Bar Foo) Foo/Bar (. Foo -Bar) into field, method group or property"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [field target op] :as ast}]
  (if (= :host-field op)
      (let [static? (= :class (:type target))
            target-type (or (and static?
                                 (-> target :val))
                            (ast-type target))
            field-name (str field)
            binding-flags (if static? public-static public-instance)
            ;; a limitation of SRE means we cannot call eg .GetProperty, on 
            ;; TypeBuilders. target-type might be a TypeBuilder so we have to do
            ;; our own filtering/lookup 
            fields (.GetFields target-type binding-flags)
            properties (.GetProperties target-type binding-flags)
            ast* (merge (dissoc ast :field)
                        (when-let [field-info (->> fields (filter #(= (.Name %) field-name)) first)]
                          {:op (if static? :static-field :instance-field)
                           :field field-info})
                        (when-let [property-info (->> properties (filter #(= (.Name %) field-name)) first)]
                          {:op (if static? :static-property :instance-property)
                           :property property-info}))]
        (if (= :host-field (:op ast*))
            (cond
             static?                     (error ::errors/missing-static-zero-arity ast)
             (or (= target-type Object)
                 (nil? target-type))     (assoc ast :op :dynamic-field)
             :else                       (error ::errors/missing-instance-field ast))
          ast*))
    ast))

;; TODO deal with parameter type conversions
;; e.g. (System.Collections.ArrayList. 12) should know to cast to int
(defn analyze-constructor
  "Analyze (Foo. a b) into object or valuetype constructor
  produces :new or :initobj"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [args children class op] :as ast}]
  (if (and (= :new op) (nil? (:type ast)))
      ;; target must be a class literal, use :val directly
      ;; (ast-type class) will always be Type here 
    (let [target-type (types/resolve (:val class))
          arg-types (map ast-type args)]
        ;; TODO OK to drop :class like this?
      (merge (dissoc ast :class)
             {:type target-type
              :children (vec (remove #(= % :class) children))}
             (cond (and (types/is-value-type? target-type)
                        (empty? args))
                   {:op :initobj}
                   ;; due to an annoying limitation of SRE we cannot look up 
                   ;; constructors in type builders, so we defer to the compiler
                   ;; in that case
                   (instance? TypeBuilder target-type)
                   {:constructor nil}
                   :else
                   (if-let [best-ctor (select-method (.GetConstructors target-type) arg-types)]
                     {:constructor best-ctor}
                     ;; a more strict implementation would use every? instead of some
                     ;; so that we only emit a dynamic constructor in the case when
                     ;; none of the types are known. probably overkill for clojure.
                     (if (some #(= Object %) arg-types)
                       {:op :dynamic-constructor
                        :type target-type}
                       (error ::errors/missing-constructor ast))))))
    ast))


(defn analyze-host-interop
  "Analyze (.foo a) (. a foo) into instance method invocation, field access, or property getter"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [m-or-f args target op] :as ast}]
  (if (= :host-interop op)
    (if (re-find #"\[" (str m-or-f))
      (throw (ex-info "Generic support disabled" {}))
      (let [identity-hack? (and (= :invoke (-> target :op))
                                (= :var (-> target :fn :op))
                                (= #'identity (-> target :fn :var))
                                (= :const (-> target :args first :op))
                                (= :class (-> target :args first :type)))
            target-type (cond
                          identity-hack?
                          System.Type
                          (and (= :const (:op target))
                               (= :class (:type target)))
                          (types/resolve (:val target))
                          :else
                          (ast-type target))
            m-or-f (str m-or-f)
            static? (= :class (:type target))
            binding-flags (if static? public-static public-instance)
            all-methods (concat (.GetMethods target-type)
                              (mapcat #(.GetMethods %) (.GetInterfaces target-type)))
            ast* (merge ast
                        (when identity-hack? ;; TODO update :form too?
                          {:target (-> target :args first)})
                        (when-let [method (first (filter #(= (.Name %) m-or-f) all-methods))]
                          {:op (if static? :static-method :instance-method)
                           :method method})
                        (when-let [field (.GetField target-type m-or-f binding-flags)]
                          {:op (if static? :static-field :instance-field)
                           :field field})
                        (when-let [property (first (filter #(= (.Name %) m-or-f) (.GetProperties target-type)))]
                          {:op (if static? :static-property :instance-property)
                           :property property}))
            matched? (not= :host-interop (:op ast*))]
        ;; NOTE for runtime dynamic dispatch (when target-type is unknown)
        ;; we will have no match, :op will become :dynamic-method or dynamic-zero-arity
        ;; also keeps :m-or-f
        (cond matched?                      (dissoc ast* :m-or-f)
              (and static? (empty? args))   (error ::errors/missing-static-zero-arity ast*)
              static?                       (error ::errors/missing-static-method ast*)
              (and target-type
                   (not= target-type Object)
                   (not static?)
                   (empty? args))           (error ::errors/missing-instance-zero-arity ast*)
              (empty? args)                 (assoc ast* :op :dynamic-zero-arity)
              :else                         (assoc ast* :op :dynamic-method))))
    ast))


;; TODO analyze away the identity invoke hack
;; this can use a review
(defn analyze-host-call
  "Analyze (Foo/Bar a) into static method invocation or (.Foo a b c) into an instance method invocation"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [method target args op] :as ast}]
  (cond
    ;; defrecord expands to this case, type did not exist at parse time, so we pick it up here
    (and (= :invoke op)
         (= :maybe-host-form (-> ast :fn :op)))
    (let [{:keys [class field]} (:fn ast)
          resolved-type (types/resolve class)
          field (str field)
          method (->> resolved-type .GetMethods (filter #(= field (.Name %))) first)]
      (if method
        (merge ast {:op :static-method
                    :method method})
        ast))
    (= :host-call op)
    (if (re-find #"\[" (str method))
      (throw (ex-info "Generic support disabled" {}))
      (let [static? (= :class (:type target))
            target-type (if static?
                          (:val target)
                          (ast-type target))]
        (if target-type
          (let [method (munge method)
                candidates (filter #(= (.Name %) (str method))
                                   (.GetMethods target-type))
                candidates' (filter #(= (.Name %) (str method))
                                    (get-all-methods target-type))]
            (merge ast
                   {:op (if static?
                          :static-method
                          :instance-method)}
                   (if-let [meth (if (and (= 1 (count candidates)) static?)
                                   (first candidates)
                                   (select-method candidates (map ast-type args)))]
                     {:method meth}
                     (if-let [best-method (select-method candidates' (map ast-type args))]
                       {:method best-method}
                       {:op (if static? 
                              :dynamic-static-method
                              :dynamic-instance-method)}))))
          (merge ast
                 {:op (if static?
                        :dynamic-static-method
                        :dynamic-instance-method)}))))
    :else
    ast))

(defn analyze-byref
  "Analyze (by-ref foo) into a CLR pass-by-reference local"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [op fn args] :as ast}]
  (if (and (= :invoke op)
           (= #'by-ref (:var fn)))
    (cond (not= 1 (count args))
          (error ::errors/by-ref-bad-arity ast)
          (not= :local (-> args first :op))
          (error ::errors/by-ref-not-local ast)
          :else
          (assoc
           (first args)
           :by-ref? true
           :load-address? true))
    ast))
(ns magic.analyzer.analyze-host-forms
  (:require
   [clojure.tools.analyzer.passes
    [uniquify :refer [uniquify-locals]]]
   [magic.analyzer
    [errors :refer [error] :as errors]
    [types :refer [read-generic-name clr-type class-for-name best-match]]])
  (:import [System.Reflection BindingFlags]))

(def public-instance (enum-or BindingFlags/Instance BindingFlags/Public))
(def public-static (enum-or BindingFlags/Static BindingFlags/Public))
(def public-instance-static (enum-or BindingFlags/Instance BindingFlags/Static BindingFlags/Public))

(defn ensure-class [c form]
  (or (class-for-name c)
      (error
       ::errors/missing-type
       {:type c :form form})))

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
                            (clr-type target))
            field-name (str field)
            binding-flags (if static? public-static public-instance)
            ast* (merge (dissoc ast :field)
                        ;; TODO is it ever a method?
                        (when-let [methods (.GetMethod target-type
                                                       field-name
                                                       binding-flags
                                                       nil
                                                       Type/EmptyTypes
                                                       nil)]
                          {:op (if static? :static-method :instance-method)
                           :methods methods})
                        (when-let [field-info (.GetField target-type field-name
                                                         binding-flags)]
                          {:op (if static? :static-field :instance-field)
                           :field field-info})
                        (when-let [property-info (.GetProperty target-type field-name
                                                               binding-flags)]
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
  (if (= :new op)
      ;; target must be a class literal, use :val directly
      ;; (clr-type class) will always be Type here 
      (let [target-type (:val class)]
        ;; TODO OK to drop :class like this?
        (merge (dissoc ast :class)
               {:type target-type
                :children (vec (remove #(= % :class) children))}
               (if (and (.IsValueType target-type)
                        (empty? args))
                   {:op :initobj}
                 (if-let [ctor-info (.GetConstructor target-type (->> args
                                                                   (map clr-type)
                                                                   (into-array Type)))]
                         {:constructor ctor-info}
                         ;; no exact match, look for arity match
                         (if-let [best-ctor (best-match args (.GetConstructors target-type))]
                                 {:constructor best-ctor}
                                 (error ::errors/missing-constructor ast))))))
    ast))

(defn analyze-generic-host-interop
  [{:keys [m-or-f args target op] :as ast}]
  (let [target-type (clr-type target)
        static? (= :class (:type target))
        binding-flags (if static? public-static public-instance)
        [method-name type-args] (read-generic-name m-or-f)
        generic-methods (->> (.GetMethods target-type)
                          (filter #(and
                                    (.IsGenericMethod %)
                                    (= (.Name %) (str method-name))
                                    (= (count (.GetParameters %))
                                       (count args)))))
        generic-method (best-match args generic-methods)]
    (if generic-method
        (assoc
         (dissoc ast :m-or-f)
         :op (if static? :static-method :instance-method)
         :method
         (.MakeGenericMethod
          generic-method
          (into-array Type (map resolve type-args))))
      (error ::errors/missing-instance-method-arity ast))))

(defn analyze-host-interop
  "Analyze (.foo a) (. a foo) into instance method invocation, field access, or property getter"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [m-or-f args target op] :as ast}]
  (if (= :host-interop op)
    (if (re-find #"\[" (str m-or-f))
      (analyze-generic-host-interop ast)
      (let [identity-hack? (and (= :invoke (-> target :op))
                               (= :var (-> target :fn :op))
                               (= #'identity (-> target :fn :var))
                               (= :const (-> target :args first :op))
                               (= :class (-> target :args first :type)))
            target-type (cond
                          identity-hack?
                          System.Type
                          (= System.Type (clr-type target))
                          (:val target)
                          :else
                          (clr-type target))
            m-or-f (str m-or-f)
            static? (= :class (:type target))
            binding-flags (if static? public-static public-instance)
            ast* (merge ast
                        (when identity-hack? ;; TODO update :form too?
                          {:target (-> target :args first)})
                        (when-let [method (.GetMethod target-type m-or-f binding-flags nil Type/EmptyTypes nil)]
                          {:op (if static? :static-method :instance-method)
                           :method method})
                        (when-let [field (.GetField target-type m-or-f binding-flags)]
                          {:op (if static? :static-field :instance-field)
                           :field field})
                        (when-let [property (.GetProperty target-type m-or-f binding-flags)]
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

(defn analyze-generic-host-call
  [{:keys [method target args op] :as ast}]
  (let [target-type (clr-type target)
        [method-name generic-args-symbols] (read-generic-name method)
        generic-args (map resolve generic-args-symbols)
        generic-methods (->> (.GetMethods target-type)
                          (filter #(and
                                    (.IsGenericMethod %)
                                    (= (.Name %) (str method-name))
                                    (= (count (.GetParameters %))
                                       (count args)))))
        generic-method (best-match args generic-methods)]
    (if generic-method
        {:generic-parameters generic-args
         :method
         (.MakeGenericMethod
          generic-method
          (into-array Type generic-args))}
      (error ::errors/missing-instance-method-arity ast))))

;; TODO analyze away the identity invoke hack
(defn analyze-host-call
  "Analyze (Foo/Bar a) into static method invocation or (.Foo a b c) into an instance method invocation"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [method target args op] :as ast}]
  (if (= :host-call op)
    (if (re-find #"\[" (str method))
      (analyze-generic-host-call ast)
      (let [static? (= :class (:type target))
            target-type (if static?
                          (:val target)
                          (clr-type target))]
        (merge ast
               {:op (if static?
                      :static-method
                      :instance-method)}
               (if-let [meth (.GetMethod target-type (str method) (->> args
                                                                       (map clr-type)
                                                                       (into-array Type)))]
                 {:method meth}
                 (if-let [best-method (best-match args (->> (.GetMethods target-type)
                                                            (filter #(and
                                                                       (= (.Name %) (str method))
                                                                       (= (count (.GetParameters %))
                                                                          (count args))))))]
                   {:method best-method}
                   (cond static?
                         ;; static method no best match, error
                         (error ::errors/missing-static-method ast)
                         ;; instance method, known target type, no best match, error
                         (and target-type
                              (not= target-type Object))
                         (error ::errors/missing-instance-method ast)
                         ;; instance method unknown target type, dynamic
                         :else
                         ;; TODO what is this generic match??
                         (let [matching-name-arity-methods
                               (->> (.GetMethods target-type)
                                    (filter #(and
                                               (.IsGenericMethod %)
                                               (= (.Name %) (str method))
                                               (= (count (.GetParameters %))
                                                  (count args)))))]
                           (if-let [best-method (best-match args matching-name-arity-methods)]
                             {:method best-method :what-is-this '???}
                             {:op :dynamic-method}))))))))
    ast))

(defn analyze-byref
  "Analyze (by-ref foo) into a CLR pass-by-reference local"
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
            :by-ref? true))
    ast))



;; TODO make into a multimethod
(defn analyze
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [ast]
  (-> ast
      analyze-byref
      analyze-type
      analyze-host-field
      analyze-constructor
      analyze-host-interop
      analyze-host-call))
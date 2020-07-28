(ns magic.analyzer.typed-passes
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.tools.analyzer.ast :refer [update-children]]
   [magic.analyzer
    [binder :refer [select-method]]
    [loop-bindings :as loop-bindings]
    [novel :as novel]
    [generated-types :as gt]
    [analyze-host-forms :as host]
    [intrinsics :as intrinsics]
    [errors :refer [error] :as errors]
    [types :refer [ast-type non-void-ast-type] :as types]]
   [magic.core :as magic]
   [magic.util :as u]
   [magic.interop :as interop]
   [magic.emission :refer [*module*]])
  (:import [System Type SByte Int16 UInt16 Int32 UInt32 Char Single IntPtr UIntPtr]
           [System.Reflection CallingConventions MethodAttributes FieldAttributes]
           System.Runtime.CompilerServices.IsVolatile))

(defn analyze-gen-interface
  [{:keys [name methods extends] :as ast}]
  (case (:op ast)
    :gen-interface
    (let [extends* (->> extends
                        (map host/analyze-type)
                        (mapv :val))
          gen-interface-type
          (gt/gen-interface-type *module* (str name) extends*)
          resolve-type
          (fn [t]
            (if (= (str t) (str name))
              gen-interface-type
              (types/resolve t)))]
      (doseq [m methods]
        (let [[name args return] m]
          (.DefineMethod
           gen-interface-type
           (str name)
           (enum-or MethodAttributes/Public MethodAttributes/Virtual MethodAttributes/Abstract)
           (resolve-type return)
           (into-array Type (mapv resolve-type args)))))
      (.CreateType gen-interface-type)
      (assoc ast :gen-interface-type gen-interface-type))
    ast))


(defn field-volatile? [f]
  (boolean (:volatile-mutable (meta f))))

(defn field-mutable? [f]
  (let [m (meta f)]
    (boolean
     (or
      (:unsynchronized-mutable m)
      (:volatile-mutable m)))))

(defn define-special-statics [tb]
  (.DefineMethod
   tb
   "getBasis"
   (enum-or MethodAttributes/Public MethodAttributes/Static)
   clojure.lang.IPersistentVector
   Type/EmptyTypes)
  ;; defrecord gets an extra static method
  (when (.IsAssignableFrom clojure.lang.IRecord tb)
    (.DefineMethod
     tb
     "create"
     (enum-or MethodAttributes/Public MethodAttributes/Static)
     tb
     (into-array Type [clojure.lang.IPersistentMap]))))

;; from https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/volatile
(defn validate-volatile-field [sym]
  (let [hint (or (types/tag sym) Object)
        valid-enum-types #{SByte Byte Int16 UInt16 Int32 UInt32}
        valid-types (into valid-enum-types [Char Single Boolean IntPtr UIntPtr])]
    (when-not (or (.IsClass hint)
                  (.IsPointer hint)
                  (valid-types hint)
                  (and (.IsEnum hint)
                       (valid-enum-types (Enum/GetUnderlyingType hint))))
      (throw (ex-info "Invalid type used as volatile field"
                      {:symbol sym :type hint :documentation "https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/volatile"})))))

(defn analyze-method [{:keys [params name] :as f} candidate-methods type-key this-type explicit-this?]
  (let [name (str name)
        name (if (string/includes? name "/")
               (subs name (inc (string/last-index-of name "/")))
               name)
        name (munge name)
        params* (if explicit-this? (drop 1 params) params)
        [interface-name method-name]
        (if (string/includes? name ".")
          (let [last-dot (string/last-index-of name ".")]
            [(subs name 0 last-dot)
             (subs name (inc last-dot))])
          [nil name])
        candidate-methods (filter #(= method-name (.Name %)) candidate-methods)
        candidate-methods (if interface-name
                            (filter #(= interface-name (.. % DeclaringType FullName)) candidate-methods)
                            candidate-methods)]
    (if-let [best-method (select-method candidate-methods (map ast-type params*))]
      (let [method-param-types (map #(.ParameterType %) (.GetParameters best-method))
            hinted-param-types (if explicit-this? 
                                 (concat [this-type] method-param-types)
                                 method-param-types)
            hinted-params (mapv #(update %1 :form vary-meta assoc :tag %2) params hinted-param-types)]
        (assoc f
               :name name
               :params hinted-params
               :source-method best-method
               type-key this-type))
      (throw (ex-info "No match binding method" {:name name :params (map ast-type params) :candidates (vec candidate-methods)
                                                 :type-key type-key :this-type this-type :explicit-this? explicit-this?})))))

(defn analyze-deftype
  [{:keys [op classname fields implements methods] :as ast}]
  (case op
    :deftype
    (let [classname (str classname)
          interfaces (->> implements
                          (map host/analyze-type)
                          (mapv :val))
          all-interfaces (into #{} (concat interfaces (mapcat #(.GetInterfaces %) interfaces)))
          candidate-methods (into #{} (concat (.GetMethods Object)
                                              (mapcat #(.GetMethods %) all-interfaces)))
          mutable-attribute FieldAttributes/Public
          immutable-attribute (enum-or mutable-attribute FieldAttributes/InitOnly)
          deftype-type
          (reduce
           (fn [t f]
             (.DefineField
              t
              (str f)
              (or (types/tag f) Object)
              (when (field-volatile? f)
                (validate-volatile-field f)
                (into-array Type [IsVolatile]))
              nil
              (if (field-mutable? f) mutable-attribute immutable-attribute))
             t)
           (gt/deftype-type *module* classname interfaces)
           fields)
          _ (.importClass *ns* deftype-type)
          methods* (mapv #(analyze-method % candidate-methods :deftype-type deftype-type true) methods)]
      (define-special-statics deftype-type)
      (assoc ast
             :deftype-type deftype-type
             :methods methods*
             :implements interfaces))
    ast))

(defn gen-il-name
  ([n] (gen-il-name n "<generated-type>"))
  ([n default]
   (string/replace
    (str "<magic>" (u/gensym (str *ns* "$" (or n default) "_")))
    "."
    "_")))

(def make-fn-type-cctor
 (memoize 
  (fn [fn-type]
    (.DefineConstructor
     fn-type
     (enum-or MethodAttributes/Public MethodAttributes/Static)
     CallingConventions/Standard
     Type/EmptyTypes))))

(defn analyze-fn
  [{:keys [op name local vars keywords variadic?] :as ast}]
  (case op
    :fn
    (let [name (or name (:form local))
          interfaces []
          fn-name (gen-il-name name "<fn>")
          fn-type (if variadic? 
                    (gt/variadic-fn-type *module* fn-name interfaces)
                    (gt/fn-type *module* fn-name interfaces))]
      (assoc ast
             :fn-type fn-type
             ;; due to a quirk of System.Reflection.Emit, we cannot query a TypeBuilder
             ;; for a static constructor. Ideally, optimization passes that need a cctor
             ;; would create one themselves or reuse one if another pass had created one
             ;; already, but the SRE quirk makes that difficult. instead we create one here
             ;; and expose it in the AST. this means that the core compiler is doing
             ;; work for the optimization passes, which is less than ideal, but what
             ;; are you going to do.
             :fn-type-cctor (when (or (pos? (count vars))
                                      (pos? (count keywords)))
                              (make-fn-type-cctor fn-type))))
    ast))

(defn analyze-proxy
  "Typed analysis of proxy forms. Generates a TypeBuilder for this proxy and
   looks up interface/super type methods. magic.emission/*module* must be bound
   before this function is called and will contain the generated proxy type when
   this function returns."
  [{:keys [op class-and-interface fns containing-fn-name] :as ast}]
  (case op
    :proxy
    (let [class-and-interface (mapv host/analyze-type class-and-interface)
          super-provided? (not (-> class-and-interface first :val .IsInterface))
          super (if super-provided?
                  (:val (first class-and-interface))
                  Object)
          interfaces (mapv :val
                           (if super-provided?
                             (drop 1 class-and-interface)
                             class-and-interface))
          interfaces* (into #{} (concat interfaces (mapcat #(.GetInterfaces %) interfaces)))
          proxy-name (gen-il-name (str containing-fn-name "<proxy>"))
          proxy-type (gt/proxy-type *module* proxy-name super interfaces)
          candidate-methods (into #{} (concat (.GetMethods super)
                                              (mapcat #(.GetMethods %) interfaces*)))
          fns (mapv #(analyze-method % candidate-methods :proxy-type proxy-type false) fns)
          closed-overs (reduce (fn [co ast] (merge co (:closed-overs ast))) {} fns)
          this-binding-name (->> closed-overs vals (filter #(= :proxy-this (:local %))) first :name)
          closed-overs (dissoc closed-overs this-binding-name)]
      (assoc ast
             :class-and-interface class-and-interface
             :super super
             :interfaces interfaces
             :closed-overs closed-overs
             :fns fns
             :proxy-type proxy-type))
    ast))

(defn analyze-reify
  [{:keys [op interfaces methods containing-fn-name] :as ast}]
  (case op
    :reify
    (let [interfaces*
          (conj
           (->> interfaces
                (map host/analyze-type)
                (mapv :val))
           clojure.lang.IObj)
          reify-name (gen-il-name (str containing-fn-name "<reify>"))
          reify-type (gt/reify-type *module* reify-name interfaces*)
          all-interfaces (into #{} (concat interfaces* (mapcat #(.GetInterfaces %) interfaces*)))
          candidate-methods (into #{} (concat (.GetMethods Object)
                                              (mapcat #(.GetMethods %) all-interfaces)))
          methods (mapv #(analyze-method % candidate-methods :reify-type reify-type true) methods)]
      (assoc ast
             :reify-type reify-type
             :interfaces interfaces*
             :methods methods))
    ast))

(defn hint-variadic-parameter [{:keys [op variadic?] :as ast}]
  (if (and (= op :binding)
           variadic?)
    (update ast :form vary-meta assoc :tag clojure.lang.ISeq)
    ast))

(defn ensure-latest-types [{:keys [op type val] :as ast}]
  (if (and (= :const op)
           (= :class type))
    (let [resolved (types/resolve (.FullName val))]
      (assoc ast :val resolved))
    ast))

(defn typed-pass* [ast]
  (-> ast
      analyze-proxy
      analyze-reify
      analyze-fn
      analyze-deftype
      analyze-gen-interface
      hint-variadic-parameter
      host/analyze-byref
      ensure-latest-types
      host/analyze-type
      host/analyze-host-field
      host/analyze-constructor
      host/analyze-host-interop
      host/analyze-host-call
      host/analyze-enums
      novel/csharp-operators
      intrinsics/analyze))

(def ^:dynamic *typed-pass-locals* {})

(def ^:dynamic *recur-expr-types* (atom []))

;; this whole thing should be rethought
(defn typed-passes [ast]
  (letfn [(update-closed-overs
            [closed-overs]
            (reduce-kv (fn [m name {:keys [local form]}]
                         (if-let [resolved (*typed-pass-locals* name)]
                           (case local
                             :proxy-this
                             (assoc-in m [name :proxy-type] resolved)
                             #_else
                             (let [form* (:form resolved)]
                               (-> m
                                   (assoc-in [name :env :locals form :init] resolved)
                                   (assoc-in [name :form] (or form* form)))))
                           m))
                       closed-overs
                       closed-overs))
          (update-bindings
            [bindings]
            (let [update-binding
                  (fn [{:keys [locals bindings]} {:keys [name] :as binding-ast}]
                    (let [binding-ast* (binding [*typed-pass-locals* locals]
                                         (typed-passes binding-ast))
                          locals* (assoc locals name binding-ast*)]
                      {:locals locals* :bindings (conj bindings binding-ast*)}))]
              (reduce update-binding {:locals *typed-pass-locals* :bindings []} bindings)))]
    (case (:op ast)
      :catch
      (let [{:keys [class local]} ast
            class* (-> class typed-passes :val)]
        (binding [*typed-pass-locals*
                  (assoc *typed-pass-locals* (:name local) class*)]
          (typed-pass* (update-children ast typed-passes))))
      :proxy-super
      (let [args (:args ast)
            env (:env ast)
            method-name (str (:method ast))
            proxy-this-binding (-> ast :env :locals (get 'this))
            this-name (:name proxy-this-binding)
            proxy-type (*typed-pass-locals* this-name)
            super-type (.BaseType proxy-type)
            candidate-methods
            (->> (.GetMethods super-type)
                 (filter #(= (.Name %) method-name)))
            args* (mapv typed-passes args)
            arg-types (map ast-type args*)]
        (if-let [best-method (select-method candidate-methods arg-types)]
          {:op :instance-method
           :non-virtual? true
           :method best-method
           :target {:op :local :local :proxy-this :proxy-type super-type :env env :form 'this}
           :args args*
           :env env
           :children [:args]}
          (throw (ex-info "Could not bind proxy-super to base class method"
                          {:method-name method-name :arg-types arg-types :form (:form ast)}))))
      :proxy
      (let [ast* (analyze-proxy ast)
            this-name (-> ast* :this-binding :name)
            proxy-type (:proxy-type ast*)]
        (binding [*typed-pass-locals* (assoc *typed-pass-locals* this-name proxy-type)]
          (let [ast** (update-children ast* typed-passes)
              ;; maybe move ths closed overs part into compiler
                closed-overs (reduce (fn [co ast] (merge co (:closed-overs ast))) {} (:fns ast**))
                this-binding-name (->> closed-overs vals (filter #(= :proxy-this (:local %))) first :name)
                closed-overs (dissoc closed-overs this-binding-name)]
            (assoc ast**
                   :closed-overs closed-overs))))
      :reify
      (let [ast* (analyze-reify ast)
            ast** (update-children ast* typed-passes)
              ;; maybe move ths closed overs part into compiler
              ;; closed-overs (reduce (fn [co ast] (merge co (:closed-overs ast))) {} (:fns ast**))
              ;; this-binding-name (->> closed-overs vals (filter #(= :proxy-this (:local %))) first :name)
              ;; closed-overs (dissoc closed-overs this-binding-name)
            ]
        (update ast** :closed-overs update-closed-overs))
      :deftype
      (let [ast* (analyze-deftype ast)]
        (update-children ast* typed-passes))
      :let
      (let [{:keys [bindings body]} ast
            {locals* :locals bindings* :bindings} (update-bindings bindings)]
        (binding [*typed-pass-locals* locals*]
          (typed-pass*
           (assoc ast
                  :bindings bindings*
                  :body (typed-passes body)))))
      :loop
      (binding [*recur-expr-types* (atom [])]
        (let [{:keys [bindings body]} ast
              {locals* :locals bindings* :bindings} (update-bindings bindings)
              binding-type-hints (mapv #(-> % :form meta :tag types/resolve) bindings)
              binding-types (mapv ast-type bindings*)
              external-incomplete-types (loop-bindings/collect-incomplete-types)
              ast* (binding [*typed-pass-locals* locals*]
                     (typed-pass*
                      (assoc ast
                             :bindings bindings*
                             :body (typed-passes body))))
              candidate-types  (->> binding-types
                                    (conj  @*recur-expr-types*)
                                    (apply map hash-set)
                                    (map #(reduce loop-bindings/best-type %))
                                    vec)
              best-types (mapv (fn [c h] (or h c)) candidate-types binding-type-hints)]
          (if (= binding-types best-types)
            ast*
            (let [bindings** (mapv (fn [binding type]
                                     (-> binding
                                         (update :form vary-meta assoc :tag type)
                                         (assoc :inferred-type type))) ;; to avoid any unwanted cache effects
                                   bindings* best-types)
                  body-sexp (:form body)
                  body-env (:env body)
                  analyzefn (find-var 'magic.analyzer/analyze)
                  {locals* :locals} (update-bindings bindings**)]
              (binding [*typed-pass-locals* locals*
                        gt/*reusable-types* (atom (set/difference (loop-bindings/collect-incomplete-types) external-incomplete-types))]
                (assoc ast
                       :bindings bindings**
                       :body (analyzefn body-sexp body-env)))))))
      :recur
      (let [ast* (update-children ast typed-passes)]
        (swap! *recur-expr-types* conj (mapv ast-type (:exprs ast*)))
        ast*)
      :local
      (let [{:keys [name form local]} ast]
        (case local
          :proxy-this
          (if-let [local-type (*typed-pass-locals* name)]
            (assoc ast :proxy-type local-type)
            (throw (ex-info "Local not found in environment"
                            {:local name :form form})))
          :catch
          (if-let [local-type (*typed-pass-locals* name)]
            (update ast :form vary-meta merge {:tag local-type})
            (throw (ex-info "Local not found in environment"
                            {:local name :form form})))
          :arg
          (if-let [init (*typed-pass-locals* name)]
            (if-not (:tag (meta form))
              (-> ast
                  (update :form vary-meta assoc :tag (-> init :form meta :tag))
                  (assoc :inferred-type (-> init :form meta :tag))) ;; to avoid any unwanted cache effects
              ast)
            ast)
          (:let :loop)
          (if-let [init (*typed-pass-locals* name)]
            (assoc-in ast [:env :locals form :init] init)
            (throw (ex-info "Local not found in environment"
                            {:local name :form form})))
          #_:else
          ast))
      :proxy-method
      (let [closed-overs (:closed-overs ast)
            closed-overs* (update-closed-overs closed-overs)
            {locals* :locals} (update-bindings (:params ast))]
        (binding [*typed-pass-locals* locals*]
          (typed-pass*
           (assoc
            (update-children ast typed-passes)
            :closed-overs closed-overs*))))
      (:reify-method :deftype-method :fn-method)
      (let [{locals* :locals} (update-bindings (:params ast))]
        (binding [*typed-pass-locals* locals*]
          (typed-pass*
           (update-children ast typed-passes))))
      (:fn :try)
      (if-let [closed-overs (:closed-overs ast)]
        (let [closed-overs* (update-closed-overs closed-overs)]
          (typed-pass*
           (update-children (assoc ast :closed-overs closed-overs*) typed-passes)))
        (typed-pass* (update-children ast typed-passes)))
      #_:else
      (typed-pass* (update-children ast typed-passes)))))
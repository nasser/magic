(ns magic.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.ast :refer [nodes prewalk postwalk update-children]]
            [clojure.tools.analyzer.utils :refer [update-vals mmerge]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [trim :refer [trim]]
             [collect-closed-overs :refer [collect-closed-overs]]
             [elide-meta :refer [elide-meta elides]]
             [warn-earmuff :refer [warn-earmuff]]
             [add-binding-atom :refer [add-binding-atom]]
             [uniquify :refer [uniquify-locals]]]
            [clojure.tools.analyzer.env :refer [*env* with-env] :as env]
            [clojure.tools.analyzer.utils :refer [resolve-sym ctx -source-info resolve-ns obj? dissoc-env]]
            [magic.analyzer
             [generated-types :as gt]
             [binder :refer [select-method]]
             [intrinsics :as intrinsics]
             [propagate-bindings :refer [propagate-bindings]]
             [util :as util]
             [literal-reinterpretation :as lr]
             [novel :as novel]
             [analyze-host-forms :as host]
             [loop-bindings :as loop-bindings]
             [remove-local-children :refer [remove-local-children]]
             [errors :refer [error] :as errors]
             [types :refer [ast-type class-for-name non-void-ast-type] :as types]]
            [clojure.walk :as w]
            [clojure.string :as string]
            [magic.core :as magic]
            [magic.interop :as interop]))

;; TODO this is duplicated in magic.analyzer.analyze-host-forms
(defn ensure-class 
  ([c] (ensure-class c c))
  ([c form]
   (or (class-for-name c)
       (and magic/*module*
            (.GetType magic/*module* (str c)))
       (error
        ::errors/missing-type
        {:type c :form form}))))

(defn maybe-class 
  ([c] (maybe-class c c))
  ([c form]
   (and c
        (or (class-for-name c)
            (and magic/*module*
                 (.GetType magic/*module* (str c)))))))

(defn desugar-host-expr [form env]
  (cond
   (symbol? form)
   (let [target (maybe-class (namespace form))]
     (if (and target
              (not (resolve-ns (symbol (namespace form)) env))
              (maybe-class target form))       ;; Class/field
       (with-meta (list '. target (symbol (str "-" (symbol (name form))))) ;; transform to (. Class -field)
         (meta form))
       form))

   (seq? form)
   (let [[op & expr] form]
     (if (symbol? op)
       (let [opname (name op)
             opns   (namespace op)]
         (cond

          (.StartsWith opname ".") ; (.foo bar ..)                                    ;;; .startsWith
          (let [[target & args] expr
                ;; wrap type literals in clojure.core/identity
                ;; disambiguates between targeting the type literal or the class
                ;; cleaned up in later analysis passes
                ;; e.g. (.GetMethods DateTime) vs (DateTime/Now)
                target (if-let [target (and (not (get (:locals env) target))
                                            (class-for-name target))]
                         (with-meta (list 'clojure.core/identity target)
                           {:tag 'System.Type})                                       ;;; java.lang.Class
                         target)
                args (list* (symbol (subs opname 1)) args)]
            (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) is
                                         (first args) args))  ;; a method call or a field access
              (meta form)))

          (and (maybe-class opns)
               (not (resolve-ns (symbol opns) env))) ; (class/field ..)
          (let [target (maybe-class opns)
                op (symbol opname)]
            (ensure-class target form)
            (with-meta (list '. target (if (zero? (count expr))
                                         op
                                         (list* op expr)))
                       (meta form)))

          (.EndsWith opname ".") ;; (class. ..)                      ;;; .endsWith
          (with-meta (list* 'new (symbol (subs opname 0 (dec (count opname)))) expr)
            (meta form))

          :else form))
       form))

   :else form))

(defn build-ns-map []
  (into {} (mapv #(vector (ns-name %)
                          {:mappings (ns-map %)
                           :aliases  (reduce-kv (fn [a k v] (assoc a k (ns-name v)))
                                                {} (ns-aliases %))
                           :ns       (ns-name %)})
                 (all-ns))))

(defn global-env []
  (atom {:namespaces (build-ns-map)}))

(def specials
  "Set of the special forms for clojure in the CLR"
  (into ana/specials
        '#{var monitor-enter monitor-exit reify* deftype* case*
           clojure.core/deftype clojure.core/definterface 
           clojure.core/proxy clojure.core/proxy-super
           clojure.core/gen-interface}))

(defn parse-monitor-enter
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-enter, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op       :monitor-enter
   :env      env
   :form     form
   :target   (ana/analyze target (ctx env :ctx/expr))
   :children [:target]})

(defn parse-monitor-exit
  [[_ target :as form] env]
  (when-not (= 2 (count form))
    (throw (ex-info (str "Wrong number of args to monitor-exit, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op       :monitor-exit
   :env      env
   :form     form
   :target   (ana/analyze target (ctx env :ctx/expr))
   :children [:target]})

(defn parse-throw
  [[_ exception :as form] env]
  (when (> (count form) 2)
    (throw (ex-info (str "Wrong number of args to throw, had: " (dec (count form)))
                    (merge {:form form}
                           (-source-info form env)))))
  {:op        :throw
   :env       env
   :form      form
   :exception (when exception (ana/analyze exception (ctx env :ctx/expr)))
   :children  [:exception]})

(defn parse-case*
  [[_ sym shift mask default imap switch-type mode skip-check :as form] env]
  (let [switch-values (->> imap keys vec)
        tests (->> imap vals (map first) vec)
        expressions (->> imap vals (map last) (map #(ana/analyze % (ctx env :ctx/expr))) vec)]
    {:op :case
     :env env
     :form form
     :local (ana/analyze (with-meta sym {}) (ctx env :ctx/expr))
     :shift shift
     :mask mask
     :default (ana/analyze default (ctx env :ctx/expr))
     :imap imap
     :switch-type switch-type
     :mode mode
     :skip-check skip-check
     :switch-values switch-values
     :tests tests
     :expressions expressions
     :children [:local :default :expressions]}))

(defn expand-proxy-method [[name & body :as form]]
  (if (vector? (first body))
    (vector form)
    (mapv (fn [[args & body]] (list* name args body)) body)))

(defn parse-proxy 
  [[_ class-and-interface args & fns :as form] env]
  (let [class-and-interface* (mapv #(ana/analyze-symbol % env) class-and-interface)
        this-binding {:op :binding :name 'this :form 'this :local :proxy-this}
        env* (assoc-in env [:locals 'this] this-binding)
        fns* (mapcat expand-proxy-method fns)]
    {:op :proxy
     :class-and-interface class-and-interface*
     :args (mapv #(ana/analyze-form % env) args)
     :this-binding this-binding
     :fns (mapv 
           #(-> (drop 1 %)
                (ana/analyze-fn-method env*)
                (assoc :name (first %)
                       :op :proxy-method))
           fns*)
     :children [:args :this-binding :fns]
     :form form
     :env env}))

(defn parse-proxy-super
  [[_ method & args :as form] env]
  {:op :proxy-super
   :method method
   :args (mapv #(ana/analyze-form % env) args)
   :children [:args]
   :form form
   :env env})

(defn parse-reify
  [[_ interfaces & methods :as form] env]
  (let [env* (assoc env :fn-method-type :reify)]
   {:op :reify
    :interfaces (mapv #(ana/analyze-symbol % env) interfaces)
    :methods (mapv
              #(-> (drop 1 %)
                   (ana/analyze-fn-method env*)
                   (assoc :name (first %)
                          :op :reify-method))
              methods)
    :meta (ana/analyze-form (meta form) env*)
    :children [:methods :meta]
    :form form
    :env env}))

(defn parse-recur
  [form {:keys [fn-method-type] :as env}]
  (case fn-method-type
    (:deftype :reify)
    (ana/parse-recur form (update env :loop-locals dec))
    (ana/parse-recur form env)))

(defn deftype-field-local [f env]
  {:op :local 
   :local :field
   :name f
   :form f
   :env env})

(defn parse-deftype*
  [[_ name classname fields & opts-specs :as form] env]
  (let [env* (->
              (reduce
               (fn [e f]
                 (assoc-in e [:locals f]
                           (deftype-field-local f env)))
               env fields)
              (assoc :fn-method-type :deftype))
        options (take-while #(keyword? (first %)) (map vec (partition 2 opts-specs)))
        options-map (into {} options)
        methods (drop (* 2 (count options)) opts-specs)]
    {:op :deftype
     :name classname
     :fields fields
     :options options-map
     :implements (mapv #(ana/analyze-symbol % env) (:implements options-map))
     :methods (mapv
               #(-> (drop 1 %)
                    (ana/analyze-fn-method env*)
                    (assoc :name (first %)
                           :op :deftype-method))
               methods)
     :form form
     :env env*
     :children [:methods]}))

(defn parse-deftype
  [[_ name fields & opts-specs :as form] env]
  (let [[interfaces methods options] (#'clojure.core/parse-opts+specs opts-specs)
        implements (conj interfaces 'clojure.lang.IType)
        env* (-> 
              (reduce 
               (fn [e f]
                 (assoc-in e [:locals f]
                           (deftype-field-local f env)))
               env fields)
              (assoc :fn-method-type :deftype))]
    {:op :deftype
     :name (str (namespace-munge *ns*) "." name)
     :fields fields
     :options options
     :implements (mapv #(ana/analyze-symbol % env) implements)
     :methods (mapv
               #(-> (drop 1 %)
                    (ana/analyze-fn-method env*)
                    (assoc :name (first %)
                           :op :deftype-method))
               methods)
     :form form
     :env env*
     :children [:methods]}))

(defn parse-gen-interface
  [[_ & options :as form] env]
  (merge
   (into {} (map vec (partition 2 options)))
   {:op :gen-interface
    :form form
    :env env}))

(defn tag-or-object [x]
  (or (-> x meta :tag)
      Object))

(defn parse-definterface-signature [[name args]]
  [name
   (mapv tag-or-object args)
   (tag-or-object name)])

(defn parse-definterface
  [[_ name & sigs :as form] env]
  (let [methods (map parse-definterface-signature sigs)]
    {:op :gen-interface
     :name name
     :methods methods
     :form form
     :env env}))

(defn parse-import*
  [[_ class-name :as form] env]
  {:op :import
   :class-name class-name
   :form form
   :env env})

(defn expand-sym [sym env]
  (when-let [val (resolve-sym sym env)]
    (if (var? val)
      (symbol (str (.. val Namespace Name)) (str (.Symbol val)))
      (symbol (str val)))))

(defn -parse [head]
  (println "[-parse]" head)
  (case head
    monitor-enter              parse-monitor-enter
    monitor-exit               parse-monitor-exit
    throw                      parse-throw
    case*                      parse-case*
    clojure.core/proxy         parse-proxy
    clojure.core/proxy-super   parse-proxy-super
    reify*                     parse-reify
    recur                      parse-recur
    clojure.core/deftype       parse-deftype
    deftype*                   parse-deftype*
    clojure.core/definterface  parse-definterface
    clojure.core/gen-interface parse-gen-interface
    clojure.core/import*       parse-import*
    #_:else                    nil))

(defn parse
  "Extension to tools.analyzer/-parse for CLR special forms"
  [form env]
  (let [head (first form)
        parsefn (or (-parse head)
                    (-parse (expand-sym head env))
                    ana/-parse)]
    (parsefn form env)))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         (ns-name *ns*)})

(defn update-ns-map! []
  (swap! *env* assoc-in [:namespaces] (build-ns-map)))

(defn macroexpand-1
  "If form represents a macro form or an inlineable function,
   returns its expansion, else returns form."
  ([form] (macroexpand-1 form (empty-env)))
  ([form env]
     (env/ensure (global-env)
       (if (seq? form)
         (let [[op & args] form]
           (if (or (specials op) (specials (expand-sym op env)))
             form
             (let [v (resolve-sym op env)
                   m (meta v)
                   local? (-> env :locals (get op))
                   macro? (and (not local?) (:macro m)) ;; locals shadow macros
                   inline-arities-f (:inline-arities m)
                   intrinsic-expr? (@intrinsics/intrinsic-forms (util/var-symbol v))
                   inline? (and (not intrinsic-expr?)
                                (not local?)
                                (or (not inline-arities-f)
                                    (inline-arities-f (count args)))
                                (:inline m))
                   t (:tag m)]
               (cond

                macro?
                (let [res' (apply v form (:locals env) (rest form)) ; (m &form &env & args)
                      ;; the case macro in the standard library explicitly tags
                      ;; the local it generates as object which messes up type flow
                      ;; we remove the tag here
                      res (if (= v #'clojure.core/case)
                            (w/prewalk #(if (and (symbol? %) (-> % meta :tag (= Object)))
                                          (with-meta % {})
                                          %)
                                       res')
                            res')] 
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge (meta form))
                    res))

                inline?
                (let [res (apply inline? args)]
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge
                               (and t {:tag t})
                               (meta form))
                    res))

                :else
                (desugar-host-expr form env)))))
         (desugar-host-expr form env)))))

(def run-passes)

;; TODO where should these go?
;; TODO clean up
#_
(defn collect-nodes
  [ast kw]
  (->> ast nodes (filter #(= collect-op (:op %)))))

(defn collect-vars
  [ast]
  (if (= :fn (:op ast))
    (assoc ast :vars (->> ast nodes (filter #(= :var (:op %)))))
    ast))

(defn collect-vectors
  [ast]
  (if (= :fn (:op ast))
    (assoc ast :vectors (->> ast nodes (filter #(= :vector (:op %)))))
    ast))

(defn enforce-var-arity
  {:pass-info {:walk :pre :before #{#'uniquify-locals}}}
  [{:keys [op fn args] :as ast}]
  (if (and (= op :invoke)
           (= (:op fn) :var))
    (let [fixed-arities (util/var-fixed-arities ast)
          variadic-arity (util/var-variadic-arity ast)
          argcount (count args)]
      (if (or (fixed-arities argcount)
              (and variadic-arity
                   (>= argcount variadic-arity)))
        ast
        (errors/error ::errors/var-bad-arity ast)))
    ast))

;; TODO is this knowledge too low level for the analyzer? 
(defn increment-arg-ids
  {:pass-info {:walk :pre :before #{#'uniquify-locals}}}
  [{:keys [local] :as ast}]
  (if (= local :arg)
    (update ast :arg-id inc)
    ast))

(def default-passes
  #{#'host/analyze-byref
    #'host/analyze-type
    #'host/analyze-host-field
    #'host/analyze-constructor
    #'host/analyze-host-interop
    #'host/analyze-host-call
    #'novel/csharp-operators
    #'novel/generic-type-syntax
    #'intrinsics/analyze
    #'propagate-bindings
    #'lr/analyze
    #'loop-bindings/infer-binding-types
    #'increment-arg-ids
    ; #'enforce-var-arity
    ; #'source-info
    ; #'collect-vars
    ; #'cleanup
    #'elide-meta
    ; #'warn-earmuff
    #'collect-vars
    #'remove-local-children
    #'collect-closed-overs
    ; #'add-binding-atom
    #'trim
    #'uniquify-locals
    })

(def ^:dynamic *stack-empty?* true)

(defn compute-empty-stack-context 
  {:pass-info {:walk :none}}
  [{:keys [op] {:keys [context]} :env :as ast}]
  (binding [*stack-empty?* 
            (or (and *stack-empty?* (not= context :ctx/expr))
                (= op :fn-method))]
    (-> ast
        (assoc-in [:env :empty-stack?] *stack-empty?*)
        (update-children compute-empty-stack-context))))

(defn remove-empty-throw-children
  {:pass-info {:walk :any :before #{#'compute-empty-stack-context}}}
  [{:keys [op exception] :as ast}]
  (case op
    :throw
    (if-not exception
      (dissoc ast :children :exception)
      ast)
    ast))

(def untyped-passes
  #{#'collect-vars
    #'remove-local-children
    #'collect-closed-overs
    #'trim
    #'uniquify-locals
    #'compute-empty-stack-context
    #'remove-empty-throw-children})

(defn analyze-gen-interface
  [{:keys [name methods extends] :as ast}]
  (case (:op ast)
    :gen-interface
    (let [extends* (->> extends
                        (map host/analyze-type)
                        (mapv :val))
          gen-interface-type
          (gt/gen-interface-type magic/*module* (str name) extends*)
          resolve-type
          (fn [t] 
            (if (= (str t) (str name)) 
              gen-interface-type
              (ensure-class t)))]
      (doseq [m methods]
        (let [[name args return] m]
          (.DefineMethod 
           gen-interface-type
           (str name)
           (enum-or System.Reflection.MethodAttributes/Public System.Reflection.MethodAttributes/Virtual System.Reflection.MethodAttributes/Abstract)
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
   (enum-or System.Reflection.MethodAttributes/Public System.Reflection.MethodAttributes/Static)
   clojure.lang.IPersistentVector
   Type/EmptyTypes))

(defn analyze-deftype
  [{:keys [op name options fields implements methods] :as ast}]
  (case op
    :deftype
    (let [name (str name)
          interfaces (->> implements
                          (map host/analyze-type)
                          (mapv :val))
          all-interfaces (into #{} (concat interfaces (mapcat #(.GetInterfaces %) interfaces)))
          candidate-methods (into #{} (concat (.GetMethods Object)
                                              (mapcat #(.GetMethods %) all-interfaces)))
          mutable-attribute System.Reflection.FieldAttributes/Public
          immutable-attribute (enum-or mutable-attribute System.Reflection.FieldAttributes/InitOnly)
          deftype-type 
          (reduce 
           (fn [t f]
             (println "[analyze-deftype]" (meta f))
             (.DefineField
              t
              (str f)
              (or (types/tag f) Object)
              (if (field-volatile? f)
                (into-array Type [System.Runtime.CompilerServices.IsVolatile])
                nil)
              nil
              (if (field-mutable? f) mutable-attribute immutable-attribute))
             t)
           (gt/deftype-type magic/*module* name interfaces)
           fields)
          methods*
          (mapv
           (fn [{:keys [params name] :as f}]
             (let [name (str name)
                   params* (drop 1 params) ;; deftype uses explicit this
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
                 (let [hinted-params (mapv #(update %1 :form vary-meta assoc :tag %2) params (concat [deftype-type] (map #(.ParameterType %) (.GetParameters best-method))))]
                   (assoc f
                          :params hinted-params
                          :source-method best-method
                          :deftype-type deftype-type))
                 (throw (ex-info "no match" {:name name :params (map ast-type params)})))))
           methods)]
      (define-special-statics deftype-type)
      (assoc ast 
             :deftype-type deftype-type
             :methods methods*
             :implements interfaces))
    ast))

(defn gen-fn-name [n]
  (string/replace
   (str (gensym
         (str *ns* "$" (or n "fn") "$")))
   "."
   "$"))

(defn analyze-fn
  [{:keys [op local methods] :as ast}]
  (case op
    :fn
    (let [name (:form local)
          param-types (->> methods
                           (map :params)
                           (mapcat #(vector (map non-void-ast-type %)
                                            (map (constantly Object) %))))
          return-types (->> methods
                            (mapcat #(vector
                                      (or (-> % :form first meta :tag types/resolve)
                                          (-> % :body non-void-ast-type))
                                      Object)))
          interfaces (map #(interop/generic-type "Magic.Function" (conj %1 %2))
                          param-types
                          return-types)
          fn-type (gt/fn-type magic/*module* (gen-fn-name name) interfaces)]
      (assoc ast :fn-type fn-type))
    ast))

(defn analyze-proxy 
  "Typed analysis of proxy forms. Generates a TypeBuilder for this proxy and
   looks up interface/super type methods. magic.core/*module* must be bound
   before this function is called and will contain the generated proxy type when
   this function returns."
  [{:keys [op class-and-interface fns] :as ast}]
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
          proxy-type (gt/proxy-type magic/*module* super interfaces)
          candidate-methods (into #{} (concat (.GetMethods super) 
                                              (mapcat #(.GetMethods %) interfaces*)))
          fns (mapv
               (fn [{:keys [params name] :as f}]
                 (let [name (str name)
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
                   (if-let [best-method (select-method candidate-methods (map ast-type params))]
                     (let [params* (mapv #(update %1 :form vary-meta assoc :tag %2) params (map #(.ParameterType %) (.GetParameters best-method)))]
                       (assoc f
                              :params params*
                              :source-method best-method
                              :proxy-type proxy-type))
                     (throw (ex-info "no match" {:name name :params (map ast-type params)})))))
               fns)
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
  [{:keys [op interfaces methods] :as ast}]
  (case op
    :reify
    (let [interfaces*
          (conj
           (->> interfaces 
                (map host/analyze-type)
                (mapv :val))
           clojure.lang.IObj)
          reify-type (gt/reify-type magic/*module* interfaces*)
          all-interfaces (into #{} (concat interfaces* (mapcat #(.GetInterfaces %) interfaces*)))
          candidate-methods (into #{} (concat (.GetMethods Object)
                                              (mapcat #(.GetMethods %) all-interfaces)))
          methods (mapv
                   (fn [{:keys [params name] :as f}]
                     (let [name (str name)
                           params* (drop 1 params) ;; reify uses explicit this
                           [interface-name method-name] 
                           (if (string/includes? name ".")
                             (let [last-dot (string/last-index-of name ".")]
                               [(subs name 0 last-dot)
                                (subs name (inc last-dot))])
                             [nil name])
                           candidate-methods (filter #(= method-name (.Name %) ) candidate-methods)
                           candidate-methods (if interface-name
                                               (filter #(= interface-name (.. % DeclaringType FullName)) candidate-methods)
                                               candidate-methods)]
                       (if-let [best-method (select-method candidate-methods (map ast-type params*))]
                         (let [hinted-params (mapv #(update %1 :form vary-meta assoc :tag %2) params (concat [reify-type] (map #(.ParameterType %) (.GetParameters best-method))))]
                           (assoc f
                                  :params hinted-params
                                  :source-method best-method
                                  :reify-type reify-type))
                         (throw (ex-info "no match" {:name name :params (map ast-type params)})))))
                   methods)]
      (assoc ast
             :reify-type reify-type
             :interfaces interfaces*
             :methods methods))
    ast))

(defn typed-pass* [ast]
  (-> ast
      analyze-proxy
      analyze-reify
      analyze-fn
      analyze-deftype
      analyze-gen-interface
      host/analyze-byref
      host/analyze-type
      host/analyze-host-field
      host/analyze-constructor
      host/analyze-host-interop
      host/analyze-host-call
      novel/csharp-operators
      novel/generic-type-syntax
      intrinsics/analyze))

(def ^:dynamic *typed-pass-locals* {})

(defn typed-passes [ast]
  (letfn [(update-closed-overs 
            [closed-overs]
            (reduce-kv (fn [m name {:keys [form]}]
                         (if-let [init (*typed-pass-locals* name)]
                           (let [form* (:form init)]
                             (-> m
                                 (assoc-in [name :env :locals form :init] init)
                                 (assoc-in [name :form] form*)))
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
      (let [{:keys [body class local]} ast
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
      (let [ast* (analyze-reify ast)]
        (println "[:reify]" (:children ast*))
        (let [ast** (update-children ast* typed-passes)
              ;; maybe move ths closed overs part into compiler
              ;; closed-overs (reduce (fn [co ast] (merge co (:closed-overs ast))) {} (:fns ast**))
              ;; this-binding-name (->> closed-overs vals (filter #(= :proxy-this (:local %))) first :name)
              ;; closed-overs (dissoc closed-overs this-binding-name)
              ]
          (update ast** :closed-overs update-closed-overs)))
      :deftype
      (let [ast* (analyze-deftype ast)]
        (update-children ast* typed-passes))
      (:let :loop)
      (let [{:keys [bindings body]} ast
            {locals* :locals bindings* :bindings} (update-bindings bindings)]
        (binding [*typed-pass-locals* locals*]
          (loop-bindings/infer-binding-types
           (assoc ast
                  :bindings bindings*
                  :body (typed-passes body)))))
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
            (do
              (println "[local :arg]" (:form ast) (-> init :form meta :tag))
              (update ast :form vary-meta assoc :tag (-> init :form meta :tag)))
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
      :reify-method
      (let [{locals* :locals} (update-bindings (:params ast))]
        (binding [*typed-pass-locals* locals*]
          (typed-pass*
           (update-children ast typed-passes))))
      :deftype-method
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

(def default-passes-opts
  "Default :passes-opts for `analyze`"
  {:collect/what                    #{:constants :callsites}
   :collect/where                   #{:reify :fn}
   :collect/top-level?              false
   :collect-closed-overs/where      #{:reify :fn :loop :try :proxy-method}
   :collect-closed-overs/top-level? false
   :uniquify/uniquify-env           true})

(def scheduled-untyped-passes
  (schedule untyped-passes))

(defn run-passes [ast]
  (scheduled-untyped-passes ast))

(defn analyze
  ([form] (analyze form (empty-env)))
  ([form env] (analyze form env (comp typed-passes run-passes)))
  ([form env passes-fn]
   (binding [ana/macroexpand-1 macroexpand-1
             ana/create-var    (fn [sym env]
                                 (doto (intern (:ns env) sym)
                                   (reset-meta! (meta sym))))
             ana/parse         parse
             ana/var?          var?]
     (env/ensure (global-env)
                 (doto (env/with-env (mmerge (env/deref-env)
                                             {:passes-opts default-passes-opts})
                         (passes-fn (ana/analyze form env)))
                   (do (update-ns-map!)))))))

(comment 
  (analyze '(. DateTime Now))
  
  (defmacro envo [a]
    `(quote ~(keys &env)))
  
  (eval
    '(let [a 99]
      (envo 9)))
  
  (clojure.core/macroexpand-1 '(let [a 12] &env))
  (clojure.core/macroexpand-1 '(. DateTime -Now))
  
  (defn same-expansion [f]
    (= (clojure.core/macroexpand-1 f) (macroexpand-1 f)))
  
  (macroexpand-1 '(. DateTime Now))
  (pprint (analyze '(.-Now DateTime)))
  
  (same-expansion '(.Now DateTime))
  
  (:op (analyze '(. "target" bar 1)))    ;; :host-call
  (:op (analyze '(. "target" (bar 1))))  ;; :host-call
  (:op (analyze '(. "target" bar)))      ;; :host-interop
  (:op (analyze '(. "foo" -bar)))        ;; :host-field
  (:op (analyze '(foo/bar 1)))           ;; :host-call
  (:op (analyze 'foo/bar))               ;; :host-field
  (:op (analyze '(.foo "target" 1)))     ;; :host-call
  (:op (analyze '(.foo "target")))       ;; :host-interop
  (:op (analyze '(.-foo "target")))      ;; :host-field
  
  (binding [*compile-path* "."]
    (compile 'aot))
  
  (aot/floo 1 2 3)
  
  (-> (analyze '(let [d (DateTime. 12)] d))
      :body
      :ret
      :env
      :locals
      (get 'd)
      :children
      pprint
      )
  
  (pprint (ast-type (analyze '(inc 3))))
  (pprint (ast-type (analyze '(int 3))))
  (pprint (analyze 'Strin))
  (pprint (analyze 'String))
  (pprint (analyze 'DateTim/Now))
  (pprint (analyze 'DateTime/Now))
  (pprint (analyze '(.-Date DateTime/Now)))
  (pprint (analyze '(Math/Sin 90)))
  (pprint (analyze '(System.Collections.ArrayList.)))
  (pprint (analyze '(System.Collections.ArrayList. (int 12))))
  (pprint (analyze '(.Reverse (System.Collections.ArrayList. (int 45)))))
  (pprint (analyze '(.. (System.Collections.ArrayList. (int 45)) ToArray Rank)))
  (pprint (analyze '(let [a 12] a)))
  (pprint (analyze '(do 1 2 3 4)))
  (pprint (analyze '(+ 1 2)))
  (pprint (analyze '(+ 1 90.1)))
  (binding [*unchecked-math* true]
    (pprint (analyze '(+ 1 2))))
  
  (require 'clojure.test)
  (require 'magic.analyzer.tests :reload)
  (clojure.test/run-tests 'magic.analyzer.tests))


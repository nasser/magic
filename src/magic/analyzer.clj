(ns magic.analyzer
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ana]
            [clojure.tools.analyzer.env :refer [*env*] :as env]
            [clojure.tools.analyzer.utils :refer [mmerge resolve-sym ctx -source-info resolve-ns obj?]]
            [magic.analyzer
             [typed-passes :refer [typed-passes]]
             [untyped-passes :refer [untyped-passes]]
             [intrinsics :as intrinsics]
             [util :as util]
             [errors :refer [error] :as errors]
             [types :as types]]
            [clojure.walk :as w]
            [magic.core :as magic]
            [magic.emission :refer [*module*]]))

(defn desugar-host-expr [form env]
  (cond
   (symbol? form)
   (let [target (types/resolve (namespace form))]
     (if (and target
              (not (resolve-ns (symbol (namespace form)) env))
              (types/resolve target))       ;; Class/field
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
                                            (types/resolve target))]
                         (with-meta (list 'clojure.core/identity target)
                           {:tag 'System.Type})                                       ;;; java.lang.Class
                         target)
                args (list* (symbol (subs opname 1)) args)]
            (with-meta (list '. target (if (= 1 (count args)) ;; we don't know if (.foo bar) is
                                         (first args) args))  ;; a method call or a field access
              (meta form)))

          (and (types/resolve opns)
               (not (resolve-ns (symbol opns) env))) ; (class/field ..)
          (let [target (types/resolve opns)
                op (symbol opname)]
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
        expressions (->> imap vals (map last) (map #(ana/analyze % env)) vec)]
    {:op :case
     :env env
     :form form
     :local (ana/analyze (with-meta sym {}) (ctx env :ctx/expr))
     :shift shift
     :mask mask
     :default (ana/analyze default env)
     :imap imap
     :switch-type switch-type
     :mode mode
     :skip-check skip-check
     :switch-values switch-values
     :tests (mapv (fn [t] {:op :const :val t}) tests)
     :expressions expressions
     :children [:local :default :expressions :tests]}))

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

(defn parse-do
  [form {:keys [fn-method-type] :as env}]
  (let [env* (case fn-method-type
               (:reify :deftype)
               (update env :loop-locals dec)
               env)]
    (ana/parse-do form (dissoc env* :fn-method-type))))

(defn deftype-field-local [f env]
  {:op :local 
   :local :field
   :name f
   :form f
   :env env})

(defn parse-deftype*
  [[_ _name classname fields & opts-specs :as form] env]
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
     :classname classname
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
  (#'clojure.core/validate-fields fields name)
  (let [[interfaces methods options] (#'clojure.core/parse-opts+specs opts-specs)
        implements (conj interfaces 'clojure.lang.IType)
        classname (str (namespace-munge *ns*) "." name)
        env* (-> 
              (reduce 
               (fn [e f]
                 (assoc-in e [:locals f]
                           (deftype-field-local f env)))
               env fields)
              (assoc :fn-method-type :deftype))]
    {:op :deftype
     :name name
     :classname classname
     :fields fields
     :options options
     :implements (mapv #(ana/analyze-symbol % env) implements)
     :methods (mapv
               #(-> (drop 1 %)
                    (ana/analyze-fn-method env*)
                    (assoc :name (first %)
                           :op :deftype-method))
               methods)
     :positional-factory (ana/analyze (#'clojure.core/build-positional-factory name classname fields) env)
     :form form
     :env env*
     :children [:methods :positional-factory]}))

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
     :name (str (namespace-munge *ns*) "." name)
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
  (case head
    monitor-enter              parse-monitor-enter
    monitor-exit               parse-monitor-exit
    throw                      parse-throw
    case*                      parse-case*
    clojure.core/proxy         parse-proxy
    clojure.core/proxy-super   parse-proxy-super
    reify*                     parse-reify
    do                         parse-do
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
                (let [inlinefn (if (ifn? inline?) 
                                 inline?
                                 (eval inline?))
                      res (apply inlinefn args)]
                  (update-ns-map!)
                  (if (obj? res)
                    (vary-meta res merge
                               (and t {:tag t})
                               (meta form))
                    res))

                :else
                (desugar-host-expr form env)))))
         (desugar-host-expr form env)))))

(def default-passes-opts
  "Default :passes-opts for `analyze`"
  {:collect/what                    #{:constants :callsites}
   :collect/where                   #{:reify :fn}
   :collect/top-level?              false
   :collect-closed-overs/where      #{:reify :fn :loop :proxy-method}
   :collect-closed-overs/top-level? false
   :uniquify/uniquify-env           true})

;; https://github.com/clojure/tools.analyzer.jvm/blob/master/src/main/clojure/clojure/tools/analyzer/jvm.clj
(defn create-var
  "Creates a Var for sym and returns it.
   The Var gets interned in the env namespace."
  [sym {:keys [ns]}]
  (let [v (get-in (env/deref-env) [:namespaces ns :mappings (symbol (name sym))])]
    (if (and v (or (class? v)
                   (= ns (ns-name (.ns ^clojure.lang.Var v)))))
      v
      (doto (intern ns sym)
        (reset-meta! (meta sym))))))

(defn analyze
  ([form] (analyze form (empty-env)))
  ([form env] (analyze form env (comp typed-passes untyped-passes)))
  ([form env passes-fn]
   (binding [ana/macroexpand-1 macroexpand-1
             ana/create-var    create-var
             ana/parse         parse
             ana/var?          var?]
     (env/ensure (global-env)
                 (doto (env/with-env (mmerge (env/deref-env)
                                             {:passes-opts default-passes-opts})
                         (passes-fn (ana/analyze form env)))
                   (do (update-ns-map!)))))))
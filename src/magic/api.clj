(ns magic.api
  (:refer-clojure :exclude [compile load-file eval])
  (:require [magic.analyzer :as ana]
            [magic.analyzer.types :refer [tag ast-type]]
            [magic.core :as magic]
            [magic.util :as u]
            [mage.core :as il]
            magic.intrinsics
            [magic.spells
             [lift-vars :refer [lift-vars]]
             [lift-keywords :refer [lift-keywords]]]
            [magic.emission :refer [*module* fresh-module]]
            [clojure.string :as string])
  (:import [clojure.lang RT LineNumberingTextReader]
           [System.IO Path Directory File]
           [System.Reflection MethodAttributes TypeAttributes]
           [System.Reflection.Emit AssemblyBuilder ModuleBuilder]))

(def empty-args (into-array []))
(def public-static (enum-or MethodAttributes/Public MethodAttributes/Static))
(def abstract-sealed (enum-or TypeAttributes/Public TypeAttributes/Abstract TypeAttributes/Sealed))

(defn bind-spells! [spells]
  (alter-var-root #'magic/*spells* (constantly spells)))

(defn bind-basic-spells! []
  (bind-spells! [lift-vars lift-keywords]))

(defn trim
  [x l]
  (let [s (str x)]
    (if (< (count s) l)
      s
      (subs s 0 l))))

(def read-options
  {:read-cond :allow
   :features #{:cljr}})

(defn compile-expression [expr ctx opts]
  (when-not (:suppress-print-forms opts)
    (println "[compile-expression]" (-> expr (trim 30)) (str *ns*) (ns-aliases *ns*)))
  (let [ast (ana/analyze expr)
        il (magic/compile ast) 
        expr-name (u/gensym (str "<magic>_" (-> *ns* str munge) "_expr"))
        expr-type (.DefineType magic.emission/*module* expr-name abstract-sealed)
        expr-method (if (:compiled-for-eval opts)
                      (.DefineMethod expr-type "eval" public-static (ast-type ast) Type/EmptyTypes)
                      (.DefineMethod expr-type "eval" public-static))
        expr-ilg (.GetILGenerator expr-method)
        ctx' (assoc ctx
                    ::il/type-builder expr-type
                    ::il/method-builder expr-method
                    ::il/ilg expr-ilg)]
    (il/emit! ctx'
              [il
               (when-not (:compiled-for-eval opts)
                 (il/pop))
               (il/ret)])
    (when-not (:compiled-for-eval opts)
      (il/emit! ctx (il/call expr-method)))
    (.CreateType expr-type)
    (.Invoke (.GetMethod expr-type "eval") nil empty-args)))

(defn compile-expression-top-level [expr ctx opts]
  (when-not (:suppress-print-forms opts)
    (println "[compile-expression-top-level]" expr))
  (cond
    (and (seq? expr)
         (= 'do (first expr)))
    (doseq [expr' (drop 1 expr)]
      (compile-expression-top-level expr' ctx opts))
    :else
    (compile-expression expr ctx opts)))

(defn clojure-clr-init-class-name
  "Port of clojure.lang.Compiler/InitClassName"
  [path]
  (str "__Init__$" (-> path (string/replace "." "/") (string/replace "/" "$"))))

(defn compile-file
  ([path module]
   (compile-file path module nil))
  ([path module opts]
   (when-not (:suppress-print-forms opts)
     (println "[compile-file] start" path))
   (let [module-name (-> module
                         str
                         (string/replace "/" ".")
                         (str ".clj"))]
     (binding [*print-meta*            false
               *ns*                    *ns*
               *file*                  path
               magic.emission/*module* (magic.emission/fresh-module module-name)]
       (let [type-name        (clojure-clr-init-class-name module)
             ns-type          (.DefineType magic.emission/*module* type-name abstract-sealed)
             init-method      (.DefineMethod ns-type "Initialize" public-static)
             init-ilg         (.GetILGenerator init-method)
             ctx              {::il/module-builder magic.emission/*module*
                               ::il/type-builder   ns-type
                               ::il/method-builder init-method
                               ::il/ilg            init-ilg}
             file             (System.IO.File/OpenText path)
             module-file-name (str module-name ".dll")]
         (try
           (let [rdr    (LineNumberingTextReader. file)
                 read-1 (fn [] (try (read read-options rdr) (catch Exception _ nil)))]
             (loop [expr (read-1) i 0]
               (when expr
                 (compile-expression-top-level expr ctx opts)
                 (recur (read-1) (inc i))))
             (.Close rdr))
           (finally
             (.Close file)))
         (il/emit! ctx (il/ret))
         (.CreateType ns-type)
         (when (:write-files opts)
           (let [compile-path (or *compile-path* ".")
                 file-name    module-file-name
                 final-path   (Path/Combine compile-path file-name)]
             (Directory/CreateDirectory compile-path)
             (if-not (File/Exists final-path)
               (let [assembly (.Assembly magic.emission/*module*)]
                 (Magic.Emission/EmitAssembly assembly file-name)
                 (when-not (= compile-path ".")
                   (File/Move file-name final-path)))
               (println "[compile-file] file already exists" final-path))
             (when-not (:suppress-print-forms opts)
               (println "[compile-file] end" path "->" final-path)))))))))

(defn compile-namespace
  "Keys in opts:
  :suppress-print-forms - if logical true, suppresses printing each compiled form"
  ([namespace]
   (compile-namespace namespace nil))
  ([namespace opts]
   (when-not (:suppress-print-forms opts)
     (println "[compile-namespace]" namespace))
   (let [relative-path (-> namespace str munge (.Replace "." "/" ))
         clj-name (str relative-path ".clj")
         cljc-name (str relative-path ".cljc")]
     (if-let [file (or (find-file clj-name) (find-file cljc-name))]
       (compile-file (.FullName file) (munge (str namespace)) opts)
       (throw (Exception. (str "could not find source file for namespace " namespace)))))))

(defn compiler-munge [s]
  (-> s (.Replace "/" ".") (.Replace "-" "_")))

(defn present-error [e]
  (loop [message (.Message e)
         data (ex-data e)
         source-span (-> data :meta :source-span)
         file (:file data)]
    (if-let [e' (:exception data)]
      (recur (.Message e') (ex-data e') (or (-> (ex-data e') :meta :source-span) source-span) (or (:file (ex-data e') file)))
      (throw (clojure.lang.ClojureException. 
              (str message (:name data) " (compiling " file ":" (:start-line source-span) ":" (:start-column source-span) ")"))))))

(defn eval [expr]
  (try
    (binding [*module* (fresh-module "eval")
              *file* "#eval#"]
      (let [ctx {::il/module-builder *module*}]
        (compile-expression-top-level
         expr ctx
         {:compiled-for-eval true
          :write-files false
          :suppress-print-forms true})))
    (catch clojure.lang.ExceptionInfo e
      (present-error e))))

(defn runtime-load-file [file path]
  (try
    (let [full-path (.FullName file)]
      (compile-file full-path (compiler-munge (str path))
                    {:write-files false :suppress-print-forms true}))
    (catch clojure.lang.ExceptionInfo e
      (present-error e))))

(defn runtime-compile-file [file path]
  (try
    (let [full-path (.FullName file)]
      (compile-file full-path (compiler-munge (str path))
                    {:write-files true :suppress-print-forms true}))
    (catch clojure.lang.ExceptionInfo e
      (present-error e))))

(defn runtime-macroexpand-1 [form]
  (ana/macroexpand-1 form))


(def version "0.0-alpha")

;; yolo
(bind-spells! [#'lift-vars #'lift-keywords])

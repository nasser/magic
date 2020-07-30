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
           [System.Reflection MethodAttributes TypeAttributes]
           [System.Reflection.Emit AssemblyBuilder ModuleBuilder]))

(def empty-args (into-array []))
(def public-static (enum-or MethodAttributes/Public MethodAttributes/Static))
(def abstract-sealed (enum-or TypeAttributes/Public TypeAttributes/Abstract TypeAttributes/Sealed))

(defn eval [expr]
  (binding [*module* (fresh-module "eval")]
    (let [ast (ana/analyze expr)
          bc (magic/compile ast)
          type-name (str (u/gensym "<magic-eval>"))]
      (->> (il/type
            type-name
            (il/method
             "eval"
             public-static
             Object []
             [bc
              (magic/convert ast Object)
              (il/ret)]))
           (il/emit! {::il/module-builder *module*}))
      (-> *module*
          (.GetType type-name)
          (.GetMethod "eval")
          (.Invoke nil empty-args)))))

(defn bind-spells! [spells]
  (alter-var-root #'magic/*spells* (constantly spells)))

(defn bind-basic-spells! []
  (bind-spells! [lift-vars lift-keywords]))

(defn find-file [roots namespace-or-path]
  (let [namespace-path (-> namespace-or-path
                           str
                           munge
                           (string/replace "." "/")
                           (string/replace "_SLASH_" "/") ;; HACK
                           (string/replace "//" "/") ;; HACK
                           )]
    (some #(let [clj-path (str % "/" namespace-path ".clj")
                 cljc-path (str % "/" namespace-path ".cljc")]
             (cond
               (System.IO.File/Exists clj-path) clj-path
               (System.IO.File/Exists cljc-path) cljc-path
               :else nil))
          roots)))

(defn trim
  [x l]
  (let [s (str x)]
    (if (< (count s) l)
      s
      (subs s 0 l))))

(declare compile-file)

;; copied from core.clj
(defn- root-resource
  "Returns the root directory path for a lib"
  {:tag String}
  [lib]
  (str \/
       (.. (name lib)
           (Replace \- \_)       ;;; replace
           (Replace \. \/))))    ;;; replace

;; copied from core.clj
(defn- root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (root-resource lib)]
    (subs d 0 (.LastIndexOf d "/"))))    ;;; lastIndexOf

;; copied from core.clj
;; copied from core.clj
(defonce ^:dynamic
  ^{:private true
    :doc "A stacj of paths currently being loaded by this thread"}
  *pending-paths* ())

(defn- check-cyclic-dependency
  "Detects and rejects non-trivial cyclic load dependencies. The
  exception message shows the dependency chain with the cycle
  highlighted. Ignores the trivial case of a file attempting to load
  itself because that can occur when a gen-class'd class loads its
  implementation."
  [path]
  (when (some #{path} (rest *pending-paths*))
    (let [pending (map #(if (= % path) (str "[ " % " ]") %)
                       (cons path *pending-paths*))
          chain (apply str (interpose "->" pending))]
      (throw (Exception. (format "Cyclic load dependency: %s" chain))))))

(def read-options
  {:read-cond :allow
   :features #{:cljr}})

(defn compile-expression [expr roots ctx]
  (println "[compile-expression]" (-> expr (trim 30)) (str *ns*) (ns-aliases *ns*))
  (let [expr-name (u/gensym "<magic>expr")
        expr-type (.DefineType magic.emission/*module* expr-name abstract-sealed)
        expr-method (.DefineMethod expr-type "eval" public-static)
        expr-ilg (.GetILGenerator expr-method)
        ctx' (assoc ctx
                    ::il/type-builder expr-type
                    ::il/method-builder expr-method
                    ::il/ilg expr-ilg)]
    (il/emit! ctx'
              [(-> expr
                   ana/analyze
                   magic/compile)
               [(il/pop)
                (il/ret)]])
    (il/emit! ctx (il/call expr-method))
    (.CreateType expr-type)
    (with-redefs [load (fn magic-load-fn [& paths]
                         ;; copied from core.clj
                         (doseq [^String path paths]
                           (let [^String path (if (.StartsWith path "/")
                                                path
                                                (str (root-directory (ns-name *ns*)) \/ path))]
                             (check-cyclic-dependency path)
                             (when-not (= path (first *pending-paths*))
                               (binding [*pending-paths* (conj *pending-paths* path)]
                                 (let [path (.Substring path 1)]
                                   ;; cant with-redefs because clojure.core/load bottoms out in
                                   ;; clojure.lang.RT/load
                                   (if-let [path' (find-file roots path)]
                                     (compile-file roots path' path)
                                     (throw (Exception. (str "Could not find " path ", roots " roots))))))))))]
      (.Invoke (.GetMethod expr-type "eval") nil empty-args))))

(defn compile-expression-top-level [expr roots ctx]
  (println "[compile-expression-top-level]" expr)
  (cond
    (and (seq? expr)
         (= 'do (first expr)))
    (doseq [expr' (drop 1 expr)]
      (compile-expression-top-level expr' roots ctx))
    :else
    (compile-expression expr roots ctx)))

(defn load-file
  [roots path ctx]
  (let [file (System.IO.File/OpenText path)]
    (try
      (let [rdr (LineNumberingTextReader. file)
            read-1 (fn [] (try (read read-options rdr) (catch Exception _ nil)))]
        (loop [expr (read-1) i 0]
          (when expr
            (compile-expression-top-level expr roots ctx)
            (recur (read-1) (inc i))))
        (.Close rdr))
      (finally
        (.Close file)))))

(defn load-assembly
  [path]
  (println "[load-assembly] start" path)
  (let [init-type
        (->> path
             assembly-load
             .GetTypes
             (filter #(.StartsWith (.Name %) "__Init__"))
             first)]
    (.Invoke (.GetMethod init-type "Initialize") nil nil))
  (println "[load-assembly] end" path))

(defn clojure-clr-init-class-name
  "Port of clojure.lang.Compiler/InitClassName"
  [path]
  (str "__Init__$" (-> path (string/replace "." "/") (string/replace "/" "$"))))

(def ^:dynamic *recompile-namespaces* false)

(def ^:dynamic *write-files* true)

(defn compile-file
  [roots path module]
  (println "[compile-file] start" path)
  (let [module-name (-> module
                        str
                        (string/replace "/" ".")
                        (str ".clj"))]
    (if (and (not *recompile-namespaces*)
             (System.IO.File/Exists (str module-name ".dll")))
      (do
        (clojure.lang.RT/load (str module))
        (println "[compile-file] end" path "(skipped, module already exists, loaded instead)"))
      (binding [*print-meta* false
                *ns* *ns*
                *file* path
                magic.emission/*module* (magic.emission/fresh-module module-name)]
        (let [type-name (clojure-clr-init-class-name module)
              ns-type (.DefineType magic.emission/*module* type-name abstract-sealed)
              init-method (.DefineMethod ns-type "Initialize" public-static)
              init-ilg (.GetILGenerator init-method)
              ctx {::il/module-builder magic.emission/*module*
                   ::il/type-builder ns-type
                   ::il/method-builder init-method
                   ::il/ilg init-ilg}]
          ;; TODO this is becoming a mess -- normalize paths in one place
          (if (System.IO.File/Exists path)
            (load-file roots path ctx)
            (if-let [path (find-file roots path)]
              (load-file roots path ctx)
              (throw (Exception. (str "Could not find " path ", roots " roots)))))
          (il/emit! ctx (il/ret))
          (.CreateType ns-type))
        (when *write-files*
         (.. magic.emission/*module* Assembly (Save (.Name magic.emission/*module*))))
        (println "[compile-file] end" path "->" (.Name magic.emission/*module*))))))

;; *loaded-libs* is supposed to be private, so we're putting this indirection
;; here for when we get var dereferencing to respect privacy
(defn- loaded-libs-ref []
  (var-get #'clojure.core/*loaded-libs*))

(defn compile-namespace
  [roots namespace]
  (println "[compile-namespace]" namespace)
  (when-let [path (find-file roots namespace)]
    (with-redefs [clojure.core/load-one (fn magic-load-one-fn [lib need-ns require]
                                          (binding [*ns* *ns*]
                                            (compile-namespace roots lib)
                                            (dosync
                                             (commute (loaded-libs-ref) conj lib))))]
      (compile-file roots path (munge (str namespace))))))

(def version "0.0-alpha")

;; yolo
(bind-spells! [#'lift-vars #'lift-keywords])

(ns magic.api
  (:refer-clojure :exclude [compile load defn eval])
  (:require [magic.analyzer :as ana]
            [magic.analyzer.types :refer [tag ast-type]]
            [magic.core :as magic]
            [mage.core :as il]
            magic.intrinsics
            [magic.spells
             [lift-vars :refer [lift-vars]]
             [dynamic-interop :refer [dynamic-interop]]]
            [magic.emission :refer [*module* fresh-module]])
  (:import [clojure.lang RT]))

(clojure.core/defn compile-asm
  ([exprs]
   (compile-asm "magic.compile" exprs))
  ([asm-name exprs]
   (compile-asm asm-name (magic/get-compilers) exprs))
  ([asm-name compilers exprs]
   (->> (map #(-> % ana/analyze (magic/compile compilers))
             exprs)
        (il/assembly+module asm-name)
        il/emit!
        ::il/assembly-builder)))

(clojure.core/defn compile-fn
  "Compile fn form using MAGIC, emit binary to current ClojureCLR compilation context
   and return the IFn instance."
  ([expr] (compile-fn expr (magic/get-compilers)))
  ([expr compilers]
   (-> expr
       ana/analyze
       (magic/compile compilers)
       il/emit!
       ::il/type-builder
       Activator/CreateInstance)))

(clojure.core/defn compile-fn-ctor [expr]
  (->> (compile-fn expr)
       .GetType
       .Name
       symbol
       (list 'new)))

(clojure.core/defn eval [expr]
  (binding [*module* (fresh-module "eval")]
    (let [ast (ana/analyze expr)
          bc (magic/compile ast)]
      (->> (il/type
            (str (gensym "magic$eval$"))
            (il/method
             "eval"
             Object []
             [bc
              (magic/convert (ast-type ast) Object)
              (il/ret)]))
           (il/emit! {::il/module-builder *module*})
           ::il/type-builders ;;  TODO MAGE bug type-builder should be available here
           vals
           first
           Activator/CreateInstance
           .eval))))


(defmacro defn
  "Compile a function using MAGIC. Useable from namespaces
   compiled by ClojureCLR."
  [name args & body]
  (let [form (list* 'fn name args body)]
    `(def ~name
       ~(compile-fn form))))

#_
(defmacro faster
  "Compile body of expression using MAGIC and emit a well-typed call site
   instead. Useable from namespaces compiled by ClojureCLR, supports closures."
  [& body]
  (let [ks (keys &env)
        vs (vals &env)
        types (map #(or (tag %1)
                        (and (.HasClrType %2) (.ClrType %2))
                        Object)
                   ks vs)
        ftype (symbol (faster-type ks types (list* 'do body)))]
    (.importClass *ns* (RT/classForName (str ftype)))
    `(. ~ftype ~'invoke ~@ks)))

(clojure.core/defn bind-spells! [spells]
  (alter-var-root #'magic/*spells* (constantly spells)))

(clojure.core/defn bind-basic-spells! []
  (bind-spells! [dynamic-interop]))

(clojure.core/defn find-file [roots namespace-or-path]
  (let [namespace-path (-> namespace-or-path
                           str
                           munge
                           (string/replace "." "/")
                           (string/replace "_SLASH_" "/") ;; HACK
                           (string/replace "//" "/") ;; HACK
                           )]
    (some #(let [path (str % "/" namespace-path ".clj")]
             (when (System.IO.File/Exists path)
               path))
          roots)))

(def public-static (enum-or MethodAttributes/Public MethodAttributes/Static))
(def abstract-sealed (enum-or TypeAttributes/Public TypeAttributes/Abstract TypeAttributes/Sealed))

(clojure.core/defn trim
  [x l]
  (let [s (str x)]
    (if (< (count s) l)
      s
      (subs s 0 l))))

(clojure.core/defn load-file
  [path ctx]
  (let [file (System.IO.File/OpenText path)]
    (try
      (let [empty-args (into-array [])
            rdr (PushbackTextReader. file)
            read-1 (fn [] (try (read rdr) (catch Exception _ nil)))]
        (loop [expr (read-1) i 0]
          (set! *warn-on-reflection* false)
          (if expr
            (do
              (println (-> expr (trim 30)) (str *ns*) (ns-aliases *ns*))
              (let [expr-name (u/gensym "expr")
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
                (.Invoke (.GetMethod expr-type "eval") nil empty-args)
                (recur (read-1) (inc i))))
            (.Close rdr))))
      (finally
        (.Close file)))))

(clojure.core/defn compile-file
  [roots path module]
  (println "[compile-file] start" path)
  (let [module-name (str module ".clj")]
    (binding [*unchecked-math* true
              *print-meta* false
              *ns* *ns*
              magic.emission/*module* (magic.emission/fresh-module module-name)]
      (let [type-name (str (gensym (clojure.string/replace path #"(\.|\/)" "_")))
            ns-type (.DefineType magic.emission/*module* type-name abstract-sealed)
            init-method (.DefineMethod ns-type "init" public-static)
            init-ilg (.GetILGenerator init-method)
            ctx {::il/module-builder magic.emission/*module*
                 ::il/type-builder ns-type
                 ::il/method-builder init-method
                 ::il/ilg init-ilg}]
        (with-redefs [load (fn magic-load-fn [& paths]
                             (println "  [load]" (vec paths))
                             (doseq [path paths]
                               (if-let [path (find-file roots path)]
                                 (load-file path ctx)
                                 (throw (Exception. (str "Could not find " path ", roots " roots))))))]
          ;; TODO this is becoming a mess -- normalize paths in one place
          (if (System.IO.File/Exists path)
            (load-file path ctx)
            (if-let [path (find-file roots path)] 
              (load-file path ctx)
              (throw (Exception. (str "Could not find " path ", roots " roots))))))
        (il/emit! ctx (il/ret))
        (.CreateType ns-type))
      (.. magic.emission/*module* Assembly (Save (.Name magic.emission/*module*)))
      (println "[compile-file] end" path))))

(def load-one' (deref (clojure.lang.RT/var "clojure.core" "load-one")))

(clojure.core/defn compile-namespace
  [roots namespace]
  (println "[compile-namespace]" namespace)
  (when-let [path (find-file roots namespace)]
    (with-redefs [clojure.core/load-one (fn magic-load-one-fn [lib need-ns require]
                                          (println "  [load-one]" lib need-ns require clojure.core/*loaded-libs*)
                                          (binding [*ns* *ns*]
                                            (compile-namespace roots lib)
                                            (dosync
                                             (commute clojure.core/*loaded-libs* conj lib))))]
      (compile-file roots path (munge (str namespace))))))

;; yolo
(bind-spells! [dynamic-interop])
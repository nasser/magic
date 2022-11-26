(ns build
  (:require [magic.core :refer [*spells*]]
            [magic.api :refer [compile-namespace]]
            [magic.spells.sparse-case :refer [sparse-case]])
  (:import [System.IO File Directory Path DirectoryInfo]))

(in-ns 'clojure.core)

;; forward declare vars added to clojure.core after 1.9 to preserve
;; bootstrapping. ideally we would just compile clojure.core first but that
;; causes its own problems.
(defmacro -forward-declare-new-vars [vars]
  `(do
     ~@(map (fn [v] `(let [vv# (declare ~v)] (println "forward declaring" vv#))) vars)))

(-forward-declare-new-vars [requiring-resolve])

(in-ns 'build)

(def std-libs-to-compile
  '[clojure.tools.analyzer.env
    clojure.tools.analyzer.utils
    clojure.tools.analyzer
    clojure.string
    clojure.set
    clojure.tools.analyzer.ast
    magic.analyzer.binder
    magic.analyzer.util
    magic.analyzer.reflection
    magic.flags
    magic.emission
    magic.analyzer.types
    magic.analyzer.generated-types
    magic.analyzer.loop-bindings
    magic.analyzer.uniquify
    magic.interop
    mage.core
    magic.util
    magic.core
    magic.spells.lift-vars
    magic.spells.lift-keywords
    magic.analyzer.errors
    magic.analyzer.analyze-host-forms
    magic.analyzer.novel
    magic.analyzer.intrinsics
    magic.analyzer.typed-passes
    clojure.tools.analyzer.passes
    clojure.tools.analyzer.passes.source-info
    clojure.tools.analyzer.passes.elide-meta
    clojure.tools.analyzer.passes.trim
    clojure.tools.analyzer.passes.cleanup
    magic.analyzer.collect-closed-overs
    magic.analyzer.remove-local-children
    magic.analyzer.untyped-passes
    clojure.walk
    magic.analyzer
    magic.analyzer.literal-reinterpretation
    magic.intrinsics
    magic.api
    clojure.core-proxy
    clojure.core-print
    clojure.genclass
    clojure.core-deftype
    clojure.core.protocols
    clojure.gvec
    clojure.clr.io
    clojure.core]) ;; if clojure.core not at the end, prevent other files from being compiled

(defn bootstrap-portable [& opts]
  (let [opts (set opts)]
    (binding [*print-meta*               true
              clojure.core/*loaded-libs* (ref (sorted-set))
              *spells*                   [sparse-case] ; (if (:portable opts) (conj *spells* sparse-case) *spells*)
              *eval-form-fn*             magic.api/eval
              *compile-file-fn*          magic.api/runtime-compile-file
              *load-file-fn*             magic.api/runtime-load-file
              *warn-on-reflection*       true
              *compile-path*             "bootstrap"]
      (doseq [lib std-libs-to-compile]
        (println (str "building " lib))
        (compile-namespace lib {:write-files true :suppress-print-forms true})))))

(defn bootstrap [& opts]
  (let [opts (set opts)]
    (binding [*print-meta*               true
              clojure.core/*loaded-libs* (ref (sorted-set))
              *eval-form-fn*             magic.api/eval
              *compile-file-fn*          magic.api/runtime-compile-file
              *load-file-fn*             magic.api/runtime-load-file
              *warn-on-reflection*       true
              *compile-path*             "bootstrap"]
      (doseq [lib std-libs-to-compile]
        (println (str "building " lib))
        (compile-namespace lib {:write-files true :suppress-print-forms true})))))

(defn move [source destination]
  (println "[moving]" source destination)
  (when (File/Exists destination)
    (File/Delete destination))
  (File/Move source destination))

(defn copy-file [source destination]
  (println "[copy-file]" source destination)
  (when (File/Exists destination)
    (File/Delete destination))
  (File/Copy source destination))

(defn copy-dir [source destination]
  (println "[copy-dir]" source destination)
  (let [dir (DirectoryInfo. source)
        dirs (.GetDirectories dir)
        files (.GetFiles dir)]
    (when-not (Directory/Exists destination)
      (Directory/CreateDirectory destination))
    (doseq [file files]
      (.CopyTo file (Path/Combine destination (.Name file)) false))
    (doseq [subdir dirs]
      (copy-dir (.FullName subdir) (Path/Combine destination (.Name subdir))))))

(defn exec [cmd args]
  (.WaitForExit (System.Diagnostics.Process/Start cmd args)))

(defn release []
  ;; build and copy runtime
  (exec "dotnet" "build Magic.Runtime/Magic.Runtime.csproj -c Debug")
  (copy-file "Magic.Runtime/bin/Debug/net35/Magic.Runtime.dll" "Magic.Unity/Infrastructure/Magic.Runtime.dll")
  ;; build il2cpp patches cli
  (exec "dotnet" "build Magic.IL2CPP/Magic.IL2CPP.CLI.csproj -c Release")
  (copy-dir "Magic.IL2CPP/bin/Release/net461" "Magic.Unity/Infrastructure/IL2CPP")
  
  ;; build clojure core
  #_(build-core)
  ;; patch clojure core for il2cpp
  (exec "mono" (str "Magic.IL2CPP/bin/Release/net461/Magic.IL2CPP.CLI.exe "
                    (String/Join " " (Directory/EnumerateFiles "." "*.clj.dll"))))
  ;; copy clojure core
  (doseq [source (Directory/GetFiles "." "clojure.*.clj.dll")]
    (let [destination (Path/Combine "Magic.Unity/Infrastructure" (Path/GetFileName source))]
      (move source destination)))
  ;; copy magic
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/magic" true)
  (copy-dir "src/magic" "Magic.Unity/Infrastructure/Desktop/magic")
  ;; copy mage
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/mage" true)
  (copy-dir "deps/github/nasser/mage-master/src/mage" "Magic.Unity/Infrastructure/Desktop/mage")
  ;; copy tools.analyzer
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/clojure" true)
  (copy-dir "deps/maven/org.clojure/tools.analyzer-1.0.0/clojure" "Magic.Unity/Infrastructure/Desktop/clojure"))

(ns build
  (:require [magic.core :refer [*spells*]]
            [magic.spells.sparse-case :refer [sparse-case]])
  (:import [System.IO File Directory Path DirectoryInfo]))

(def std-libs-to-compile
  '[clojure.string ;; respect this order to prevent compiling twice the same file
    clojure.walk
    clojure.edn
    clojure.set
    clojure.spec.alpha
    clojure.core.specs.alpha
    clojure.main
    clojure.pprint
    clojure.clr.io
    clojure.clr.shell
    clojure.core.protocols
    clojure.core.reducers
    clojure.core.server
    clojure.data
    clojure.instant
    clojure.repl
    clojure.template
    clojure.stacktrace
    clojure.test
    clojure.uuid
    clojure.zip
    magic.api
    clojure.core]) ;; if clojure.core not at the end, prevent other files from being compiled

(defn bootstrap [& opts]
  (let [opts (set opts)]
    (binding [*print-meta* true
              clojure.core/*loaded-libs* (ref (sorted-set))
              *spells* (if (:portable opts) (conj *spells* sparse-case) *spells*)
              *eval-form-fn* magic.api/eval
              *compile-file-fn* magic.api/runtime-compile-file
              *load-file-fn* magic.api/runtime-load-file
              *warn-on-reflection* true
              *compile-path* "bootstrap"]
      (doseq [lib std-libs-to-compile]
        (println (str "building " lib))
        (compile lib)))))

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

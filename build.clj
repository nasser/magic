(ns build
  (:require [magic.api :as api]
            magic.util
            #_files
            [magic.core :refer [*spells*]]
            [magic.spells.sparse-case :refer [sparse-case]]
            #_[mage.core :as il]
            #_[magic.profiler :refer [profile write-trace!]])
  (:import [System.Reflection MethodAttributes TypeAttributes BindingFlags]
           [System.IO File Directory Path DirectoryInfo]))

#_(def local-load-paths
    [files/magic-root
     files/mage-root
     files/clojure-root
     files/analyzer-root
     (str files/clojure-root "clojure") ;; HACK
     files/test-root
     "."
     "/home/nasser/projects/magic/sandbox"
     "/home/nasser/projects/magic/datascript/src"
     "/home/nasser/projects/magic/datascript/test"
     "/home/nasser/projects/arcadia/Assets/Arcadia/Source/"])

(defn bootstrap [& opts]
  (let [opts (set opts)]
    (binding [*print-meta* true
              clojure.core/*loaded-libs* (ref (sorted-set))
              *load-paths* (vec (concat [] *load-paths*))
              *eval-form-fn* magic.api/eval
              *compile-file-fn* magic.api/runtime-compile-file
              *load-file-fn* magic.api/runtime-load-file
              *spells* (if (:portable opts) (conj *spells* sparse-case) *spells*)
              *warn-on-reflection* true
              *compile-path* "bootstrap"]
      (compile 'clojure.core)
      (println (str "build 'clojure.core"))
      (compile 'clojure.spec.alpha)
      (println (str "build 'clojure.spec.alpha"))
      (compile 'clojure.core.specs.alpha)
      (println (str "build 'clojure.core.specs.alpha"))
      (compile 'clojure.pprint)
      (println (str "build 'clojure.pprint"))
      (compile 'clojure.clr.io)
      (println (str "build 'clojure.clr.io"))
      (compile 'clojure.clr.shell)
      (println (str "build 'clojure.clr.shell"))
      (compile 'clojure.core.protocols)
      (println (str "build 'clojure.core.protocols"))
      (compile 'clojure.core.reducers)
      (println (str "build 'clojure.core.reducers"))
      (compile 'clojure.core.server)
      (println (str "build 'clojure.core.server"))
      (compile 'clojure.data)
      (println (str "build 'clojure.data"))
      (compile 'clojure.edn)
      (println (str "build 'clojure.edn"))
      (compile 'clojure.instant)
      (println (str "build 'clojure.instant"))
      (compile 'clojure.main)
      (println (str "build 'clojure.main"))
      (compile 'clojure.repl)
      (println (str "build 'clojure.repl"))
      (compile 'clojure.set)
      (println (str "build 'clojure.set"))
      (compile 'clojure.stacktrace)
      (println (str "build 'clojure.stacktrace"))
      (compile 'clojure.string)
      (println (str "build 'clojure.string"))
      (compile 'clojure.template)
      (println (str "build 'clojure.template"))
      (compile 'clojure.test)
      (println (str "build 'clojure.test"))
      (compile 'clojure.uuid)
      (println (str "build 'clojure.uuid"))
      (compile 'clojure.walk)
      (println (str "build 'clojure.walk"))
      (compile 'clojure.zip)
      (println (str "build 'clojure.zip"))
      (compile 'magic.api))))

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

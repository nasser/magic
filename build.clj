(ns build
  (:require [magic.api :as api]
            magic.util
            files
            [mage.core :as il]
            [magic.profiler :refer [profile write-trace!]])
  (:import [System.Reflection MethodAttributes TypeAttributes BindingFlags]
           [System.IO File Directory Path DirectoryInfo]))

(def load-path
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

(defn build-core []
  (binding [*print-meta* true
            *warn-on-reflection* false
            clojure.core/*loaded-libs* (ref (sorted-set))
            magic.api/*recompile-namespaces* true]
    (api/compile-namespace load-path 'clojure.core)
    (api/compile-namespace load-path 'clojure.spec.alpha)
    (api/compile-namespace load-path 'clojure.core.specs.alpha)
    (api/compile-namespace load-path 'clojure.core.server)
    (api/compile-namespace load-path 'clojure.edn)
    (api/compile-namespace load-path 'clojure.data)
    (api/compile-namespace load-path 'clojure.set)))

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
  (build-core)
  ;; patch clojure core for il2cpp
  (exec "mono" (str "Magic.IL2CPP/bin/Release/net461/Magic.IL2CPP.CLI.exe "
                    (String/Join " " (Directory/EnumerateFiles "." "*.clj.dll"))))
  ;; copy clojure core
  (doseq [source (Directory/GetFiles "." "clojure.*.clj.dll")]
    (let [destination (Path/Combine "Magic.Unity/Infrastructure" (Path/GetFileName source))]
      (move source destination)))
  ;; copy magic`
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/magic" true)
  (copy-dir "src/magic" "Magic.Unity/Infrastructure/Desktop/magic")
  ;; copy mage
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/mage" true)
  (copy-dir "deps/github/nasser/mage-master/src/mage" "Magic.Unity/Infrastructure/Desktop/mage")
  ;; copy tools.analyzer
  (Directory/Delete "Magic.Unity/Infrastructure/Desktop/clojure" true)
  (copy-dir "deps/maven/org.clojure/tools.analyzer-1.0.0/clojure" "Magic.Unity/Infrastructure/Desktop/clojure"))
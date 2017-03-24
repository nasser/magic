(ns build
  (:require magic.constants
            magic.interfaces))

(defn references
  "Rebuild Constants.dll and Interfaces.dll"
  []
  (print "Constants.dll...")
  (magic.constants/emit!)
  (println "OK")
  (print "Interfaces.dll...")
  (magic.interfaces/emit!)
  (println "OK"))
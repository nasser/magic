(ns magic.analyzer.remove-local-children
  (:require
   [clojure.tools.analyzer.passes.collect-closed-overs :refer [collect-closed-overs]]))

(defn remove-local-children
  {:pass-info {:walk :any :before #{#'collect-closed-overs}}}
  [ast]
  (case (:op ast)
    :local (dissoc ast :children)
    ast))
(ns magic.analyzer.propagate-bindings
  (:require
   [clojure.tools.analyzer.ast :refer [prewalk]]
   [magic.analyzer [intrinsics :as intrinsics]]))

(defn propagate-bindings
  "Propagate bindings into locals"
  {:pass-info {:walk :post :after #{#'intrinsics/analyze}}}
  [{:keys [op] :as ast}]
  (case op
    (:let :loop)
    (let [binding-map (into {} (map (fn [{:keys [name init]}] [name init]) (:bindings ast)))]
      (prewalk 
       ast
       (fn [{:keys [op name] :as ast}]
         (case op
           :local
           (if-let [init (binding-map name)]
             (assoc ast :init init)
             ast)
           ast))))
    ast))
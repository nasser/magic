(ns magic.analyzer.intrinsics
  (:require
    [clojure.tools.analyzer.passes
     [uniquify :refer [uniquify-locals]]]
    [magic.analyzer
     [util :as util]
     [errors :refer [error] :as errors]
     [types :refer [ast-type numeric integer]]])
  (:import [System.Reflection BindingFlags]))

(def intrinsic-forms (atom {}))

(defn register-intrinsic-form [sym type-fn il-fn]
  (if (namespace sym)
    (swap! intrinsic-forms assoc sym {::type type-fn ::il il-fn})
    (throw (ex-info "Must use fully qualified symbol" {:got sym}))))

(defn analyze
  "Analyze invoke forms into CLR intrinsics if possible"
  {:pass-info {:walk :post :after #{#'uniquify-locals}}}
  [{:keys [op fn args form raw-forms] :as ast}]
  (let [intrinsic-ast
        (when (and (= op :invoke) (:var fn))
          (when-let [bc-fn (@intrinsic-forms (util/var-symbol (:var fn)))]
            (when-let [bc-type ((::type bc-fn) ast)]
              (merge ast
                     {:op :intrinsic
                      :il-fn (::il bc-fn) ;; TODO this is basically a "compiler"... need better name
                      :type bc-type
                      :original ast}))))]
    ;; TODO reapply inlining if possible when falling back to original ast
    (or intrinsic-ast
        ast)))
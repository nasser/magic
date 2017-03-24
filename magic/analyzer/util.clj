(ns magic.analyzer.util)

;; TODO replace with error multimethod
(defmacro throw! [& e]
  `(throw (Exception. (str ~@e))))

(defn var-reference
  "Get the var reference associated with an AST, or nil"
  [{:keys [init var] :as ast}]
  (cond
    ;; var literal
    var var 
    ;; local
    init (:var init)))

(defn var-interfaces
  "The interfaces the var associated with the AST implements, or []"
  [ast]
  (if-let [v (var-reference ast)]
    (-> v deref type .GetInterfaces)
    []))

(ns magic.analyzer.loop-bindings
  (:require
   [clojure.set :as s]
   [clojure.tools.analyzer.ast :as ast]
   [magic.analyzer.types :refer [ast-type numeric-type? best-numeric-promotion]]
   [magic.analyzer.generated-types :refer [*reusable-types*]]
   [magic.emission :refer [*module*]]))

(defn gather-recur-asts [ast]
  (let [children (->> ast ast/children (remove #(= :loop (:op %))))]
    (-> #{}
        (into (filter #(= :recur (:op %)) children))
        (into (mapcat #(gather-recur-asts %) children)))))

(defn update-locals [f ast]
  (if (= :local (:op ast))
    (f ast)
    (ast/update-children ast #(update-locals f %))))

(defn type-depth
  ([t] (type-depth t 0))
  ([t d] (if (or (nil? t) (= t Object) (.IsInterface t))
           d
           (recur (.BaseType t) (inc d)))))

(defn best-type [a b]
  (if (and (numeric-type? a) (numeric-type? b))
    (best-numeric-promotion [a b])
    (let [common-ancestors   (s/intersection
                              (conj (or (ancestors a) #{}) a)
                              (conj (or (ancestors b) #{}) b))
          common-types       (remove #(.IsInterface %) common-ancestors)
          common-interfaces  (filter #(.IsInterface %) common-ancestors)
          deepest-base-type  (when-not (empty? common-types)
                               (reduce
                                (fn [t1 t2]
                                  (if (> (type-depth t2) (type-depth t1)) t2 t1))
                                common-types))
          deepest-base-type  (if (= deepest-base-type ValueType) Object deepest-base-type)
          heaviest-interface (when-not (empty? common-interfaces)
                               (reduce
                                (fn [t1 t2]
                                  (if (> (count (.GetInterfaces t2))
                                         (count (.GetInterfaces t1)))
                                    t2 t1))
                                common-interfaces))]
      (or deepest-base-type heaviest-interface Object))))

(defn safe-resolve [x]
  (when x
    (if (symbol? x)
      (resolve x)
      x)))


(defn collect-incomplete-types []
  (->> *module*
       .GetTypes
       (filter #(instance? System.Reflection.Emit.TypeBuilder %))
       (remove #(.IsCreated %))
       (into #{})))

(defn infer-binding-types
  "Collect the best types for loop bindings

   loop bindings are complicated by the fact that recur expressions effectively
   write to the local variables created by the loop expression. That means that
   the type of the variable needs to be assignable to from the type of every recur
   expression."
  [{:keys [op bindings body] :as ast}]
  (if (= :loop op)
    (let [analyzefn        (find-var 'magic.analyzer/analyze)
          recurs           (gather-recur-asts ast)
          binding-types    (mapv ast-type bindings)
          binding-type-hints (map #(-> % :form meta :tag safe-resolve) bindings)
          recur-expr-types (->> recurs
                                (map :exprs)
                                (map #(map ast-type %)))
          candidate-types  (->> binding-types
                                (conj recur-expr-types)
                                (apply map hash-set)
                                (map #(reduce best-type %))
                                vec)
          best-types (mapv (fn [c h] (or h c)) candidate-types binding-type-hints)]
      (if (= binding-types best-types)
        ast
        (let [sexpr-body (:form body)
              body-env (:env body)
              bindings' (mapv (fn [binding type]
                                (-> binding
                                    (update :form vary-meta assoc :tag type)
                                    (assoc :inferred-type type))) ;; to avoid any unwanted cache effects
                              bindings best-types)
              binding-map (into {} (mapv vector (map :form bindings) best-types))
              body-env' (assoc body-env :locals
                          (reduce-kv
                           (fn [m k v] (if-let [better-type (binding-map k)]
                                         (assoc m k (update v :form vary-meta assoc :tag better-type))
                                         m))
                           (:locals body-env)
                           (:locals body-env)))
              body' (binding [*reusable-types* (atom (collect-incomplete-types))]
                      (analyzefn sexpr-body body-env'))]
          (assoc ast
                 :body body'
                 :bindings bindings'))))
    ast))
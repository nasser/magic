(ns magic.faster
  (:require [clojure.tools.analyzer.clr :as ana]
            [clojure.tools.analyzer.clr.types :refer [clr-type best-match]]
            [magic.core :as magic]
            [mage.core :as il])
  (:import [clojure.lang RT]
           [System.Reflection
            TypeAttributes
            MethodAttributes
            FieldAttributes
            FieldInfo
            MethodInfo
            PropertyInfo]))

(defn generic-params [n]
  (->> (range \A (+ \A n))
       (map (comp symbol str char))))

(defn generic-method [n]
  (let [params (generic-params n)
        ret (first params)
        args (vec (rest params))]
    (il/method
      "invoke"
      (enum-or MethodAttributes/Public
               MethodAttributes/Abstract
               MethodAttributes/Virtual)
      ret args [])))

(defn generic-interface [name n]
  (let [params (generic-params n)
        ret (first params)
        args (vec (rest params))]
    (il/type
      (str name "`" n)
      (enum-or TypeAttributes/Interface
               TypeAttributes/Abstract
               TypeAttributes/Public)
      []
      nil
      params
      [(generic-method n)])))

(comment
  
  ;; 1. helpers
  ;; emits class with well typed static methods
  (defstatic Helpers
    (add [^Int32 a ^Int32 b]
         (+ a b))
    (distance [^Vector3 from ^Vector3 to]
              (Vector3/Distance from to)))
  
  ;; usage from magic or clojure
  (Helpers/add 1 2)
  
  
  ;; 2. magic functions
  ;; defined var bound to magic function
  (defmagic foo [^int a ^float b]
    (Vector3/* (Vector3. a a a) b))
  
  ;; usage from clojure cast to IFn and box, usage form magic wont
  (foo 4 5)
  
  
  ;; 3. faster macro
  ;; faster macro is usable from normal clojure and will emit good bytecode
  (defn normal-clojure [a b]
    (let [pos-a (.. (GameObject/Find a) transform position)
          pos-b (.. (GameObject/Find b) transform position)]
      (faster
        (Vector3/Lerp pos-a pos-b 0.5)))))

(defn faster-type [args-names args-types body]
  (let [name (gensym "Faster")
        wrapped-body `(clojure.core/fn
                        ~(->> args-names
                              (map #(vary-meta %2 assoc :tag %1)
                                   args-types)
                              vec)
                        ~body)
        body-ast (ana/analyze wrapped-body)
        body-expr (-> body-ast :methods first :body)
        static-arg-symbolizers
        (assoc (or @#'magic/*initial-symbolizers* magic/base-symbolizers) ;; TODO this should be in core
          :local
          (fn [{:keys [name arg-id local] :as ast} symbolizers]
            (if (= local :arg)
              (magic/load-argument arg-id)
              (magic/throw! "Local " name " not an argument and could not be symbolized"))))
        body-il (magic/symbolize body-expr static-arg-symbolizers)
        type-il
        (il/type
          (str name)
          (il/method
            "Invoke"
            (enum-or MethodAttributes/Public
                     MethodAttributes/Static)
            (clr-type (body-expr :ret))
            (vec args-types)
            [body-il
             (il/ret)]))]
    (il/emit! type-il)
    name))

(defmacro faster [& body]
  (let [ks (keys &env)
        vs (vals &env)
        types (map #(or (if-let [t (-> %1 meta :tag)] (resolve t))
                        (and (.HasClrType %2)
                             (.ClrType %2))
                        Object)
                   ks vs)
        ftype (faster-type ks types (list* 'do body))]
     (.importClass *ns* (RT/classForName (str ftype)))
    `(. ~ftype ~'Invoke ~@ks)))
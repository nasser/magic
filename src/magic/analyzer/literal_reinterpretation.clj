(ns magic.analyzer.literal-reinterpretation
  (:require
    [clojure.tools.analyzer.passes
     [uniquify :refer [uniquify-locals]]]
    [magic.analyzer
     [analyze-host-forms :as host]
     [util :as util]
     [errors :refer [error] :as errors]
     [types :refer [ast-type numeric integer]]])
  (:import [System.Reflection BindingFlags]))

;; TODO idk if this is in the right place
(defn reinterpret-value [val to-type]
  (let [v (condp = to-type
            Single (Convert/ToSingle val)
            Double (Convert/ToDouble val)
            Byte (Convert/ToByte val)
            SByte (Convert/ToSByte val)
            Int16 (Convert/ToInt16 val)
            UInt16 (Convert/ToUInt16 val)
            Int32 (Convert/ToInt32 val)
            UInt32 (Convert/ToUInt32 val)
            Int64 (Convert/ToInt64 val)
            UInt64 (Convert/ToUInt64 val)
            val)]
    v))

;; TODO is this better than e.g. a peephope pass?
(defn reinterpret [{:keys [literal? op val] :as ast} to-type]
  (if (and (= op :const) literal?)
    (let [v (reinterpret-value val to-type)]
      (assoc ast :val v :form v :const-type (type v)))
    ast))

(defn reinterpret-interop [ast method-key args-key]
  (if-let [method (method-key ast)]
    (let [args (args-key ast)
          param-types (->> method
                           .GetParameters
                           (map #(.ParameterType %)))
          args* (vec (map #(reinterpret %1 %2)
                          args
                          param-types))]
      (assoc ast :args args*))
    ast))

(defn analyze
  "Reinterpret numeric literals to avoid casts when possible"
  {:pass-info {:walk :post :after #{#'host/analyze-byref
                                    #'host/analyze-type
                                    #'host/analyze-host-field
                                    #'host/analyze-constructor
                                    #'host/analyze-host-interop
                                    #'host/analyze-host-call}}}
  [{:keys [op] :as ast}]
  (condp = op
    :set!
    (let [{:keys [val target]} ast]
      (update ast :val reinterpret (ast-type target)))
    :static-method
    (reinterpret-interop ast :method :args)
    :instance-method
    (reinterpret-interop ast :method :args)
    :new
    (reinterpret-interop ast :constructor :args)
    ;; TODO reinterpret :invoke for Magic functions
    ast))
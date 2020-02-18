(ns magic.test.common
  (:require [magic.api :as m])
  (:use clojure.test))

(defmacro cljclr=magic [& exprs]
  `(do ~@(map (fn [expr] `(is (= ~expr (m/eval ~expr)))) exprs)))
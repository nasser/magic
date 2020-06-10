(ns magic.util
  (:refer-clojure :exclude [gensym]))

(def ^:dynamic *gensym-map* (atom {}))

(defn gensym [base]
  (let [base (str base)]
    (str base "_"
         (get (swap! *gensym-map* update base #(if % (inc %) 0))
              base))))

(defmacro reset-gensym [keys & body]
  `(binding [*gensym-map* (atom (dissoc @*gensym-map* ~keys))]
     ~@body))
(ns magic.spells.sparse-case)

(defn sparse-case 
  "Forces case expressions to compile sparsely when not switching on
   integers. Avoids embedding hash results in the bytecode which breaks
   across runtimes."
  [compilers]
  (update
   compilers
   :case
   (fn
     [old-case-compiler]
     (fn sparse-case-compiler
       [{:keys [mode] :as ast} compilers]
       (let [ast' (if (= mode :int) ast
                      (assoc ast :switch-type :sparse))]
         (old-case-compiler ast' compilers))))))
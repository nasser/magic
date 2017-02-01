(ns magic.optimizations)

(defn sq-replace
  [n match? replacement sq]
  ((fn replace-in-sequence [[elt & elts :as sq]]
     (lazy-seq
       (cond (empty? sq)
             ()
             (and (>= (count sq) n)
                  (apply match? (take n sq)))
             (concat (apply replacement (take n sq)) (replace-in-sequence (drop n sq)))
             :default
             (cons elt (replace-in-sequence elts)))))
   sq))

(defn peephole [il]
  (->> il
       (sq-replace
         2
         #(and (= (::il/opcode %1) OpCodes/Ldc_I8)
               (= (::il/opcode %2) OpCodes/Conv_I4))
         (fn [{:keys [::il/argument]} b] [(il/ldc-i4 (int argument))]))
       (sq-replace
         2
         #(and (= (::il/opcode %1) OpCodes/Ldc_I8)
               (= (::il/opcode %2) OpCodes/Conv_R4))
         (fn [{:keys [::il/argument]} b] [(il/ldc-r4 (float argument))]))))
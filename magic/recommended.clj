(ns magic.recommended
  (:require [magic.core :as mc]
            magic.spells.intrinsics
            magic.spells.lift-vars))

(defn setup-spells! []
  (mc/ensure-spell!
    magic.spells.intrinsics/intrinsics
    magic.spells.lift-vars/lift-vars))

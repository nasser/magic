(ns magic.test.proxy
  (:require [clojure.test :refer [deftest]])
  (:use magic.test.common))

;; using identity to drop type information

(deftest direct-interop
  (cljclr=magic
   (.count (proxy [clojure.lang.Counted] []
             (count [] (int 9))))
   (.Next (proxy [Random] [] (Next [] (int 9))))
   (let [p (proxy [Random clojure.lang.Counted] [] 
             (count [] (int 8))
             (Next [] (int 9)))]
     (+ (.count p) (.Next p)))))

(deftest base-class-arguments
  (cljclr=magic
   (.Next (proxy [Random] [90]))
   (let [x 90]
     (.Next (proxy [Random] [x])))))

(deftest nested-proxy
  (cljclr=magic
   (.Next (proxy [Random] []
            (Next [] (.Next (proxy [Random] []
                              (Next [] (int 9)))))))))

(deftest proxy-super-works
  (cljclr=magic
   (let [x 90]
     (.Next
      (proxy [Random] [x]
        (Next [] (int (* (.NextDouble this) (proxy-super Next)))))))
   (.Next
    (proxy [Random] [(int 0)]
      (Next [] (int (+ (proxy-super NextDouble) (proxy-super NextDouble) (proxy-super NextDouble))))))
   (.Next
    (proxy [Random] [(int 0)]
      (Next [] (proxy-super Next) (proxy-super Next) (proxy-super Next) (proxy-super Next))))
   (.Next
    (proxy [Random] [(int 0)]
      (Next [] (int (+ 1 (try (proxy-super Next) (catch Exception e 0)))))))))
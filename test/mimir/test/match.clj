(ns mimir.test.match
  (:use [mimir.match :only (defm condm match)]
        [mimir.test.common]
        [clojure.walk :only (postwalk prewalk walk postwalk-replace)]
        [clojure.test]))

; scratchpad, real tests yet to be written

(defm member? [x & y]
  [x & _ ]  true
  [_ & xs]  (member? x xs))

(defm filter-m [pred & coll]
  [^x pred & xs] (cons x (filter-m pred xs))
  [_       & xs] (filter-m pred xs)
  empty?         ())

(defm map-m [f & coll]
  [x & xs] (cons (f x) (map-m f xs)))

(defm reduce-m [f val & coll]
  [x & xs] (reduce-m f (f x val) xs)
  empty?   val)

(defn factorial [x]
  (condm x
         0 1
         x (* x (factorial (dec x)))))

;; (defm factorial [& x]
;;          0 1
;;          x (* x (factorial (dec x))))

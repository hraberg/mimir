(ns mimir.test.match
  (:use [mimir.match :only (defm condm match)]
        [mimir.test.common]
        [clojure.test]))

; scratchpad, real tests yet to be written

(defm member? [x & y]
  [x & _ ]  true
  [_ & xs]  (member? x xs))

(defm filter-2 [pred & coll]
  [^x pred & xs] (cons x (filter-2 pred xs))
  [_       & xs] (filter-2 pred xs)
  empty?         ())

(defm map-2 [f & coll]
  [x & xs] (cons (f x) (map-2 f xs)))

(defm reduce-2 [f val & coll]
  [x & xs] (reduce-2 f (f x val) xs)
  empty?   val)

(defn factorial [x]
  (condm x
         0 1
         x (* x (factorial (dec x)))))

;; (defm factorial [& x]
;;          0 1
;;          x (* x (factorial (dec x))))
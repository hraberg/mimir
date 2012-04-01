(ns mimir.test.common
  (:use [mimir.well :only (run triplets quote-fact fact reset *net*)]
        [clojure.set :only (subset?)]
        [clojure.test]))

(defmacro match? [& expected]
  (when expected
    `(is (= (set ~(vec (triplets expected quote-fact))) (set (run))))))

(defmacro matches? [& expected]
  (when expected
    `(is (subset? ~(set expected) (set (run))))))

(defmacro ? [& expected]
  `(match? ~@expected))

(defn with-reset-fixture []
  (use-fixtures :each (fn [f] (reset) (f) (reset))))

(defn with-integers []
  (->> (range 10) (map fact) doall))

(defn base* [base [x & xs]]
  (when x
    (cons `(* ~@(repeat (count xs) 10) ~x) (base* base xs))))

(defmacro base [base & xs]
  `(+ ~@(base* base xs)))

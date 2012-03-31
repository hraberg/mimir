(ns mimir.test.common
  (:use [mimir.well :only (run triplets quote-fact fact reset *net*)]
        [clojure.set :only (subset?)]
        [clojure.test]))

(defmacro match? [& expected]
  (when expected
    `(is (= (set ~(vec (triplets expected quote-fact))) (set (run))))))

(defmacro in-match? [& expected]
  (when expected
    `(is (subset? ~(set expected) (set (run))))))

(defmacro ? [& expected]
  `(match? ~@expected))

(defn with-reset-fixture []
  (use-fixtures :each (fn [f] (reset) (f) (reset))))

(defn with-integers []
  (->> (range 10) (map fact) doall))

(defn coef [& [x & xs]]
  (if x
    (->> (Math/pow 10 (count xs)) (* x)
         (+ (apply coef xs)) int)
    0))

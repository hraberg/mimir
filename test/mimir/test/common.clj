(ns mimir.test.common
  (:use [mimir.well :only (run triplets quote-fact fact reset *net*)]
        [clojure.set :only (subset? difference)]
        [clojure.test]))

(defmacro match? [& expected]
  (when expected
    `(is (= (set ~(vec (triplets expected quote-fact))) (set (run))))))

(defn no-matches? []
  (match?))

(defmacro matches? [& expected]
  (when expected
    `(is (subset? ~(set expected) (set (run))))))

(defmacro ? [& expected]
  `(match? ~@expected))

(defn with-reset-fixture []
  (use-fixtures :each (fn [f] (reset) (f) (reset))))

(defn integers []
  (->> (range 10) (map fact) doall))

(defn base* [base [x & xs]]
  (when x
    (cons `(* ~@(repeat (count xs) 10) ~x) (base* base xs))))

(defmacro base [base & expr]
  (let [[x [op] y [test] z] (partition-by '#{+ - * / =} expr)
        mod-test ('{+ <= * <= - >= / >=} op)]
    (concat
     (for [[a b c] (partition 3 (interleave (reverse x) (reverse y) (reverse z)))]
       `(~mod-test (mod (~op ~a ~b) ~base) ~c))
     (list `(~test (~op (+ ~@(base* base x)) (+ ~@(base* base y))) (+ ~@(base* base z)))))))

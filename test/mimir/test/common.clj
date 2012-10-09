(ns mimir.test.common
  (:use [mimir.well :only (run parser quote-fact fact reset *net*)]
        [clojure.set :only (subset? difference)]
        [clojure.test]))

(defmacro match? [& expected]
  (when expected
    `(is (= (set ~(vec (parser expected identity quote-fact false))) (run)))))

(defn no-matches? []
  (match?))

(defmacro matches? [& expected]
  (when expected
    `(is (subset? ~(set expected) (set (run))))))

(defn with-reset-fixture []
  (use-fixtures :each (fn [f] (reset) (require '[mimir.match :reload true]) (f) (reset))))

(defn integers
  ([] (integers 0 9))
  ([start end]  (->> (range start (inc end)) (map fact) doall)))

(defmacro base [base & expr]
  (let [[x [op] y [test] z] (partition-by '#{+ =} expr)
        reminders (map (comp symbol (partial str "?") gensym) (reverse z))]
    (concat
     [`(> ~(first x) 0) `(> ~(first y) 0)]

     (if (= (count z) (count x))
       [`(> ~(first z) 0) `(= ~(last reminders) 0)]
       [`(= ~(first z) ~(last (butlast reminders)))])

     (for [r reminders]
       `(or (= 0 ~r) (= 1 ~r)))

     (for [[carry a b c c-rem] (partition 5 (interleave (cons 0 reminders)
                                                        (reverse x)
                                                        (reverse y)
                                                        (reverse z)
                                                        reminders))]
       `(= (~op ~carry ~a ~b) (~op ~c (* ~base ~c-rem)))))))

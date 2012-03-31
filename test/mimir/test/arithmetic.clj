(ns mimir.test.arithmetic
  (:use [mimir.well :only (rule run fact facts all-different?)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(deftest simple-arithmetic
  (with-integers)

  (rule xyz
        (< ?x ?y)
        (> ?z 5)
        (= ?z (+ ?x ?y))
        (not= ?z ?y)
        =>
        (str ?x '+ ?y '= ?z))

  (matches? "2+4=6"))

(deftest send-more-money
  (with-integers)

  (rule send-more-money
        (> ?s 0)
        (= ?m 1)

        (= (+ (coef ?s ?e ?n ?d)
              (coef ?m ?o ?r ?e))
           (coef ?m ?o ?n ?e ?y))

        (all-different? ?s ?e ?n ?d ?m ?o ?r ?y)

        =>

        (str ?s ?e ?n ?d '+ ?m ?o ?r ?e '= ?m ?o ?n ?e ?y))

   (time (match? "9567+1085=10652")))


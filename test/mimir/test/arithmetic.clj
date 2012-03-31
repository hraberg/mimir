(ns mimir.test.arithmetic
  (:use [mimir.well :only (rule run fact facts)]
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

  (in-match? "2+4=6"))

(deftest send-more-money
  (with-integers)

  (rule send-more-money
        (> ?s 0)
        (= ?m 1)

        (== (+ (coef ?s ?e ?n ?d)
               (coef ?m ?o ?r ?e))
            (coef ?m ?o ?n ?e ?y))

        =>

        (str ?s ?e ?n ?d '+ ?m ?o ?r ?e '= ?m ?o ?n ?e ?y))

   (in-match? "9563+1095=10658"))


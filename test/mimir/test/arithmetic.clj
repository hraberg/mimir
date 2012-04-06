(ns mimir.test.arithmetic
  (:use [mimir.well :only (rule run fact facts all-different)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(deftest simple-arithmetic
  (integers)

  (rule xyz
        X < Y
        Z > 5
        Z = (+ X Y)
        Z != Y
        =>
        (str X '+ Y '= Z))

  (matches? "2+4=6"))

(deftest equals
  (integers)

  (rule equals
        X = Y
        =>
        (str X '= Y))

  (matches? "1=1"))

(deftest send-more-money
  (integers)

  (rule send-more-money
        (base 10    S E N D
                  + M O R E
                = M O N E Y)

        (all-different S E N D M O R Y)

        =>

        (str S E N D '+ M O R E '= M O N E Y))

  (time (match? "9567+1085=10652")))

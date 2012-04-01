(ns mimir.test.arithmetic
  (:use [mimir.well :only (rule run fact facts all-different)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(deftest simple-arithmetic
  (integers)

  (rule xyz
        (< X Y)
        (> Z 5)
        (= Z (+ X Y))
        (not= Z Y)
        =>
        (str X '+ Y '= Z))

  (matches? "2+4=6"))

(deftest send-more-money
  (integers)

  (rule send-more-money
        (> S 0)
        (= M 1)

        (-> (+ (base 10     S E N D)
               (base 10     M O R E))
            (= (base 10   M O N E Y)))

        (all-different S E N D M O R Y)

        =>

        (str S E N D '+ M O R E '= M O N E Y))

  (time (match? "9567+1085=10652")))

(ns mimir.test.golfers
  (:use [mimir.well :only (rule run facts)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(deftest golfers
  (rule find-solution)

;; (defrule find-solution
;;   ;; There is a golfer named Fred, whose position is ?p1
;;   ;; and pants color is ?c1
;;   (position (of Fred) (is ?p1))
;;   (pants-color (of Fred) (is ?c1))

;;   ;; The golfer to immediate right of Fred
;;   ;; is wearing blue pants.
;;   (position (of ?n&~Fred)(is ?p&:(eq ?p (+ ?p1 1))))
;;   (pants-color (of ?n&~Fred)(is blue&~?c1))

;;   ;; Joe is in position #2
;;   (position (of Joe) (is ?p2&2&~?p1))
;;   (pants-color (of Joe) (is ?c2&~?c1))

;;   ;; Bob is wearing the plaid pants
;;   (position (of Bob)(is ?p3&~?p1&~?p&~?p2))
;;   (pants-color (of Bob&~?n)(is plaid&?c3&~?c1&~?c2))

;;   ;; Tom is not in position 1 or 4
;;   ;; and is not wearing orange
;;   (position (of Tom&~?n)(is ?p4&~1&~4&~?p1&~?p2&~?p3))
;;   (pants-color (of Tom)(is ?c4&~orange&~blue&~?c1&~?c2&~?c3))

;;   =>
;;   (printout t Fred " " ?p1 " " ?c1 crlf)
;;   (printout t Joe " " ?p2 " " ?c2 crlf)
;;   (printout t Bob " " ?p3 " " ?c3 crlf)
;;   (printout t Tom " " ?p4 " " ?c4 crlf)
;; )
  )


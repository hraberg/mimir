(ns mimir.test.golfers
  (:use [mimir.well :only (rule run fact different all-different)]
        [mimir.test.common]
        [clojure.test])
  (:refer-clojure :exclude [assert]))

(with-reset-fixture)

(deftest golfers
  (doseq [name ["Fred" "Joe" "Bob" "Tom"]
          pants-color [:red :blue :plaid :orange]
          position (range 1 (inc 4))]
    (fact {:name name :position position :pants-color pants-color}))

  (rule find-solution
        (= "Fred" (:name ?g1))

        (= (:position ?g) (inc (:position ?g1)))
        (= :blue (:pants-color ?g))

        (= "Joe" (:name ?g2))
        (= 2 (:position ?g2))

        (= "Bob" (:name ?g3))
        (= :plaid (:pants-color ?g3))

        (= "Tom" (:name ?g4))
        (not= 1 (:position ?g4))
        (not= 4 (:position ?g4))
        (not= :orange (:pants-color ?g4))

        (some #(= ?g %) [?g1 ?g2 ?g3 ?g4])

        (different :position
                   ?g1 ?g2 ?g3 ?g4)

        (different :pants-color
                   ?g1 ?g2 ?g3 ?g4)

        =>
        (hash-set ?g1 ?g2 ?g3 ?g4))

  (match? #{{:name "Fred", :position 1, :pants-color :orange}
            {:name "Joe", :position 2, :pants-color :blue}
            {:name "Bob", :position 4, :pants-color :plaid}
            {:name "Tom", :position 3, :pants-color :red}})

;; Fred 1 orange
;; Joe 2 blue
;; Bob 4 plaid
;; Tom 3 red

  )

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


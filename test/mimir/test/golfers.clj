(ns mimir.test.golfers
  (:use [mimir.well :only (rule run fact different)]
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

        (some #{?g} [?g1 ?g2 ?g3 ?g4])

        (different :position
                   ?g1 ?g2 ?g3 ?g4)

        (different :pants-color
                   ?g1 ?g2 ?g3 ?g4)

        =>

        #{?g1 ?g2 ?g3 ?g4})

  (match? #{{:name "Fred", :position 1, :pants-color :orange}
            {:name "Joe", :position 2, :pants-color :blue}
            {:name "Bob", :position 4, :pants-color :plaid}
            {:name "Tom", :position 3, :pants-color :red}}))

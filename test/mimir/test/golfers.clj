(ns mimir.test.golfers
  (:use [mimir.well :only (rule run fact different not-in is-not)]
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
        ?g1 {:name "Fred"}

        ?g  {:position (-> ?g1 :position inc)
             :pants-color :blue}

        ?g2 {:name "Joe"
             :position 2}

        ?g3 {:name "Bob"
             :pants-color :plaid}

        ?g4 {:name "Tom"
             :position (not-in #{1 4})
             :pants-color (is-not :orange)}

        ?golfers <- #{?g1 ?g2 ?g3 ?g4}

        (different #{:position :pants-color} ?golfers)

        (contains? ?golfers ?g)

        =>

        (set ?golfers))

  (time (match? #{{:name "Fred", :position 1, :pants-color :orange}
                  {:name "Joe", :position 2, :pants-color :blue}
                  {:name "Bob", :position 4, :pants-color :plaid}
                  {:name "Tom", :position 3, :pants-color :red}})))

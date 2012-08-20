(ns mimir.test.golfers
  (:use [mimir.well :only (rule run fact different all-different not-in is-not constrain)]
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
        {:name "Fred"
         :position fred}

        {:name "Joe"
         :position 2}

        {:name "Bob"
         :pants-color :plaid}

        {:name "Tom"
         :position (not-in #{1 4})
         :pants-color (is-not :orange)}

        (constrain {:position (inc ?fred)
                    :pants-color :blue})

        (different #{:position :pants-color})

        =>

        (set *matches*))

  (time (match? #{{:name "Fred", :position 1, :pants-color :orange}
                  {:name "Joe", :position 2, :pants-color :blue}
                  {:name "Bob", :position 4, :pants-color :plaid}
                  {:name "Tom", :position 3, :pants-color :red}})))

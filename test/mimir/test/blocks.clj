(ns mimir.test.blocks
  (:use [mimir.well :only (rule run facts fact retract working-memory alpha-network)]
        [mimir.test.common]
        [clojure.set :only (difference)]
        [clojure.test]))

(with-reset-fixture)

(deftest blocks
  (facts B1 on B2
         B1 on B3
         B1 color red
         B2 on table
         B2 left-of B3
         B2 color blue
         B3 left-of B4
         B3 on table
         B3 color red)

  (rule find-stack-of-two-blocks-to-the-left-of-a-red-block
        ?x on ?y
        ?y left-of ?z
        ?z color red
        =>
        ?x is on-top)

  (match? B1 is on-top)

  ; retract irrelevant fact
  (retract B2 color blue)
  (match? B1 is on-top)

  ; retract relevant fact
  (let [an ((alpha-network) '(?1 left-of ?2))]

    (retract B2 left-of B3)

    (no-matches?)
    (is (= '#{{?1 B2 ?2 B3}}
           (difference an ((alpha-network) '(?1 left-of ?2)))))

    ; restate the fact
    (facts B2 left-of B3)

    (match? B1 is on-top)
    (is (= an ((alpha-network) '(?1 left-of ?2))))))
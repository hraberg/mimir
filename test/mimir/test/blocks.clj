(ns mimir.test.blocks
  (:use [mimir.well :only (rule run facts fact retract working-memory)]
        [mimir.test.common]
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
  (working-memory-size-is 9)
  (match? B1 is on-top)

  ; retract relevant fact
  (retract B2 left-of B3)
  (working-memory-size-is 8)
  (no-matches?)

  ; restate the fact
  (facts B2 left-of B3)
  (working-memory-size-is 9)
  (match? B1 is on-top))
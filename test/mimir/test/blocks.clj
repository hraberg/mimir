(ns mimir.test.blocks
  (:use [mimir.well :only (rule run facts)]
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

  (match? B1 is on-top))

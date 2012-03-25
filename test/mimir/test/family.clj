(ns mimir.test.family
  (:use [mimir.well :only (rule facts working-memory)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(deftest family
  (facts John son Dan
         Mary sister Suzan
         Harold brother Larry
         John married Mary
         Larry married Sue
         Larry son John)

  (rule father
        ?x son ?y
        =>
        ?y father ?x)

  (rule grandfather
        ?x father ?y
        ?y father ?z
        =>
        ?z grandfather ?x)

  (rule sister-in-law
        ?x married ?y
        ?y sister ?z
        =>
        ?z sister-in-law ?x)

  (match? Suzan sister-in-law John
          Dan father John
          John father Larry
          Larry grandfather Dan))

(deftest socrates
  (facts Socrates is human)

  (rule mortal
        ?x is human
        =>
        ?x is mortal)

  (match? Socrates is mortal)

  (is (= '#{(Socrates is human)
            (Socrates is mortal)} (working-memory))))

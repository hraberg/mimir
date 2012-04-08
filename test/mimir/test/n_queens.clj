(ns mimir.test.n-queens
  (:use [mimir.well :only (rule fact unique not-same different)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(defn chessboard [n]
  (let [size (range 1 (inc n))]
    (doall (for [x size y size]
             (fact [x y])))))

(defn rank [x] (x 0))
(defn file [x] (x 1))

(defn diagonal? [x y]
  (= (Math/abs (- (rank x) (rank y)))
     (Math/abs (- (file x) (file y)))))

(deftest n-queens
  (chessboard 5)

  (rule five-queens
        ?queens <- (unique [?q1 ?q2 ?q3 ?q4 ?q5])

        (different file ?queens)

        (different rank ?queens)

        (not-same diagonal? ?queens)

        =>

        (map file ?queens))

  (time (match? [4 2 5 3 1] [3 5 2 4 1] [5 3 1 4 2] [4 1 3 5 2] [5 2 4 1 3]
                [1 4 2 5 3] [2 5 3 1 4] [1 3 5 2 4] [3 1 4 2 5] [2 4 1 3 5])))
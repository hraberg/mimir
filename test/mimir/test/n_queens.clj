(ns mimir.test.n-queens
  (:use [mimir.well :only (rule fact take-unique not-same different gen-vars)]
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

(def ^:dynamic *n* 5)

(deftest n-queens-test
  (chessboard *n*)

  (rule n-queens

        ?queens <- (take-unique *n*)
        (different #{file rank} ?queens)
        (not-same diagonal? ?queens)

        =>

        (map file ?queens))

  (time (match? [4 2 5 3 1] [3 5 2 4 1] [5 3 1 4 2] [4 1 3 5 2] [5 2 4 1 3]
                [1 4 2 5 3] [2 5 3 1 4] [1 3 5 2 4] [3 1 4 2 5] [2 4 1 3 5])))
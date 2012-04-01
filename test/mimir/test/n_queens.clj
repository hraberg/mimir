(ns mimir.test.n-queens
  (:use [mimir.well :only (rule fact unique?)]
        [mimir.test.common]
        [clojure.test]))

(with-reset-fixture)

(defn chessboard [n]
  (let [size (range 1 (inc n))]
    (doall (for [x size y size]
             (fact [x y])))))

(defn rank [x] (x 0))
(defn file [x] (x 1))

(defn rank? [x y] (= (rank x) (rank y)))
(defn file? [x y] (= (file x) (file y)))

(defn diagonal? [x y]
  (= (Math/abs (- (rank x) (rank y)))
     (Math/abs (- (file x) (file y)))))

(defn no [pred & xs]
  (not-any? true? (for [x xs y (remove #{x} xs)]
                    (pred x y))))

(deftest n-queens
  (chessboard 4)

  (rule four-queens

        (unique? ?N1 ?N2 ?N3 ?N4)

        (no file?
            ?N1 ?N2 ?N3 ?N4)

        (no rank?
            ?N1 ?N2 ?N3 ?N4)

        (no diagonal?
            ?N1 ?N2 ?N3 ?N4)

        =>

        (map file [?N1 ?N2 ?N3 ?N4]))

  (match? [3 1 4 2]
          [2 4 1 3]))

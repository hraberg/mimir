(ns mimir.test.n-queens
  (:use [mimir.well :only (rule fact unique not-same)]
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

(deftest n-queens
  (chessboard 4)

  (rule four-queens

        (unique [?q1 ?q2 ?q3 ?q4])

        (not-same file?
                  ?q1 ?q2 ?q3 ?q4)

        (not-same rank?
                  ?q1 ?q2 ?q3 ?q4)

        (not-same diagonal?
                  ?q1 ?q2 ?q3 ?q4)

        =>

        (map file [?q1 ?q2 ?q3 ?q4]))

  (match? [3 1 4 2]
          [2 4 1 3]))

;; (deftest five-queens
;;   (chessboard 5)

;;   (rule five-queens

;;         (not-same file?
;;                   ?q1 ?q2 ?q3 ?q4 ?q5)

;;         (not-same rank?
;;                   ?q1 ?q2 ?q3 ?q4 ?q5)

;;         (not-same diagonal?
;;                   ?q1 ?q2 ?q3 ?q4 ?q5)

;;         (unique [?q1 ?q2 ?q3 ?q4 ?q5])

;;         =>

;;         (map file [?q1 ?q2 ?q3 ?q4 ?q5]))

;;   (time (match? [4 2 5 3 1] [3 5 2 4 1] [5 3 1 4 2] [4 1 3 5 2] [5 2 4 1 3]
;;                 [1 4 2 5 3] [2 5 3 1 4] [1 3 5 2 4] [3 1 4 2 5] [2 4 1 3 5])))
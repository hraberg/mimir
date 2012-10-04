(ns mimir.test.mk
  (:use [mimir.mk]
        [mimir.match :only (*match-var?*)]
        [mimir.test.common]
        [clojure.test])
  (:refer-clojure :exclude [reify var? ==]))

(def mv *match-var?*)
(alter-var-root #'*match-var?* (constantly var?))

(deftest introduction-to-core-minikanren
  (are [a _ e] (is (= a e))

       (run 1 [q]
         (exist [x y z]
           (≡ x z)
           (≡ 3 y))) ⇒ '(–₀)

       (run 1 [y]
         (exist [x z]
           (≡ x z)
           (≡ 3 y))) ⇒ '(3)

       (run 1 [q]
         (exist [x z]
           (≡ x z)
           (≡ 3 z)
           (≡ q x))) ⇒ '(3)

       (run 1 [y]
         (exist [x y]
           (≡ 4 x)
           (≡ x y))
         (≡ 3 y))    ⇒ '(3)

       (run 1 [y]
         (≡ 3 4))    ⇒ ()

       (run 2 [q]
         (exist [x y z]
           (condᵉ
             ((≡ [x y z x] q))
             ((≡ [z y x z] q)))))
                     ⇒ '((–₀ –₁ –₂ –₀) (–₀ –₁ –₂ –₀)))

  (is (= '((a 1 d) (b 2 e) (c 3 f))
         (run 5 [q]
           (exist [x y z]
             (condᵉ
               ((≡ 'a x) (≡ 1 y) (≡ 'd z))
               ((≡ 2 y) (≡ 'b x) (≡ 'e z))
               ((≡ 'f z) (≡ 'c x) (≡ 3 y)))
             (≡ [x y z] q)))))

  (defn anyᵒ [g]
    (condᵉ
      (g)
      ((anyᵒ g))))

  ;;StackOverflow
  ;; (are [a _ e] (is (= a e))

  ;;      (sort (run 5 [q]
  ;;              (condᵉ
  ;;                ((anyᵒ (≡ false q)))
  ;;                ((≡ true q)))))
  ;;                    ⇒ '(false false false false true)

  ;;       (run 10 [q]
  ;;            (anyᵒ
  ;;             (condᵉ
  ;;              ((≡ 1 q))
  ;;              ((≡ 2 q))
  ;;              ((≡ 3 q)))))
  ;;                      ⇒ '(1 2 3 1 2 3 1 2 3 1)
  ;;      )
 )

(deftest consᵒ-the-magnificent
  (are [a _ e] (is (= a e))

       (run* [q]
         (consᵒ 1 [2 3] q))
                     ⇒ '((1 2 3))

       (run* [q]
         (consᵒ 1 q [1 2 3]))
                     ⇒ '((2 3))

       (run* [q]
         (consᵒ q [2 3] [1 2 3]))
                     ⇒ '(1)

       (run* [q]
         (consᵒ 1 [2 q] [1 2 3]))
                     ⇒ '(3)

       (run* [q w]
         (consᵒ q w [1 2 3]))
                     ⇒ '(1 (2 3))

       (run* [q w z]
         (consᵒ q w z))
                     ⇒ '(–₀ –₁ (–₀ . –₁))

       (run* [q]
         (restᵒ [1 2 3 4] q))
                     ⇒ '((2 3 4))

       (run* [q]
         (restᵒ q [2 3 4]))
                     ⇒ '((–₀ 2 3 4))


       (run* [q]
         (firstᵒ [1 2 3 4] q))
                     ⇒ '(1)

       (run* [q]
         (firstᵒ q 1))
                     ⇒ '((1 . –₀))))

(alter-var-root #'*match-var?* (constantly mv))
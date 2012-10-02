(ns mimir.test.mk
  (:use [mimir.mk]
        [mimir.match :only (*match-var?*)]
        [mimir.test.common]
        [clojure.test])
  (:refer-clojure :exclude [reify var? ==]))

(def mv *match-var?*)
(alter-var-root #'*match-var?* (constantly var?))

(use-fixtures :once (fn [suite]
                      (binding [mimir.match/*match-var?* var?]
                        (suite))))

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
            (≡ 3 y))   ⇒ '(3)

       (run 1 [y]
            (≡ 3 4))   ⇒ ()

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

  ;; StackOverflow
  ;; (is ([false false false false true]
  ;;        (sort (run 5 [q]
  ;;                   (condᵉ
  ;;                    ((anyᵒ (≡ false q)))
  ;;                    ((≡ true q)))))))
  )


(alter-var-root #'*match-var?* (constantly mv))
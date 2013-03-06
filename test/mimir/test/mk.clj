(ns mimir.test.mk
  (:use [mimir.mk]
        [mimir.match :only (*match-var?*)]
        [clojure.test])
  (:refer-clojure :exclude [reify var? ==]))

;; Mess of compile time and runtime requires this atm:
(def mv *match-var?*)
(alter-var-root #'*match-var?* (constantly var?))

(deftest ^:mk introduction-to-core-minikanren
  (are [a _ e] (is (= a e))

       (run 1 [q]
         (fresh [x y z]
           (≡ x z)
           (≡ 3 y))) ⇒ '(–₀)

       (run 1 [y]
         (fresh [x z]
           (≡ x z)
           (≡ 3 y))) ⇒ '(3)

       (run 1 [q]
         (fresh [x z]
           (≡ x z)
           (≡ 3 z)
           (≡ q x))) ⇒ '(3)

       (run 1 [y]
         (fresh [x y]
           (≡ 4 x)
           (≡ x y))
         (≡ 3 y))    ⇒ '(3)

       (run 1 [y]
         (≡ 3 4))    ⇒ ()

       (run 2 [q]
         (fresh [x y z]
           (condᵉ
             ((≡ [x y z x] q))
             ((≡ [z y x z] q)))))
                     ⇒ '((–₀ –₁ –₂ –₀) (–₀ –₁ –₂ –₀)))

  ;; StackOverflow when compiling using are, something about symbols
  (is (= '((a 1 d) (b 2 e) (c 3 f))
         (run 5 [q]
           (fresh [x y z]
             (condᵉ
               ((≡ 'a x) (≡ 1 y) (≡ 'd z))
               ((≡ 2 y) (≡ 'b x) (≡ 'e z))
               ((≡ 'f z) (≡ 'c x) (≡ 3 y)))
             (≡ [x y z] q)))))

  (defn anyᵒ [g]
    (condᵉ
      (g)
      ((anyᵒ g))))

  (def alwaysᵒ (anyᵒ succeed))
  (def neverᵒ (anyᵒ fail))

  (are [a _ e] (is (= a e))

       (run 5 [q]
            (condᵉ
             ((anyᵒ (≡ false q)))
             ((≡ true q))))
                  ⇒ '(false true false false false)

        ;; Does order matter? Returns nested interleave: (1 1 2 1 3 2 1 3 2 1)
        ;; (run 10 [q]
        ;;      (anyᵒ
        ;;       (condᵉ
        ;;        ((≡ 1 q))
        ;;        ((≡ 2 q))
        ;;        ((≡ 3 q)))))
        ;;                ⇒ '(1 2 3 1 2 3 1 2 3 1)


       ;; StackOverflow
       ;; (run 5 [x]
       ;;   (condᵉ
       ;;    ((≡ true x))
       ;;    ((≡ false x)))
       ;;   alwaysᵒ
       ;;   (≡ false x))
       ;;            ⇒ '(false false false false false)

       ;; (run 3 [q]
       ;;   (condᵉ
       ;;     ((≡ 1 q))
       ;;     (neverᵒ)
       ;;     ((condᵉ
       ;;       ((≡ 2 q))
       ;;       (neverᵒ)
       ;;       ((≡ 3 q))))))
       ;;            ⇒ '(1 2 3)

       (run* [q]
         (fresh [x]
           (≡ 5 x)
           (project [x]
             (≡ (* x x) q))))
                  ⇒ '(25)))

(deftest ^:mk unification
  (are [a _ e] (is (= a e))

       (run* [q]
         (≡ 5 5)) ⇒ '(–₀)

       (run* [q]
         (≡ 5 4)) ⇒ ()

       (run* [q]
         (≡ 5 q)
         (≡ 5 q)) ⇒ '(5)

       (run* [q]
         (≡ 5 q)
         (≡ 4 q)) ⇒ ()

      ;; works, but not in suite with *match-var*? mess
      ;; (run* [q]
      ;;    (fresh [x y]
      ;;      (≡ [x 2] [1 y])
      ;;      (≡ q [x y])))
      ;;             ⇒ '([1 2])

      (run* [q]
        (fresh [x y]
           (≡ x y)
           (≡ q [x y])))
                  ⇒ '([–₀ –₀])

      (run* [q]
        (fresh [x y]
          (≡ x y)
          (≡ y 1)
          (≡ q [x y])))
                  ⇒ '([1 1])))

(deftest ^:mk consᵒ-the-magnificent
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

       (run* [q w z]
         (consᵒ q w z)
         (≡ q 1))
                     ⇒ '(1 –₀ (1 . –₀))

       (run* [q w z]
         (consᵒ q w z)
         (firstᵒ z 1))
                     ⇒ '(1 –₀ (1 . –₀))

       (run* [q w z]
         (consᵒ q w z)
         (≡ w [2 3]))
                     ⇒ '(–₀ (2 3) (–₀ 2 3))

       (run* [q w z]
         (consᵒ q w z)
         (restᵒ z [2 3]))
                     ⇒ '(–₀ (2 3) (–₀ 2 3))

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
                     ⇒ '((1 . –₀))

       (run* [q]
         (consᵒ 1 [2 3] q)
         (firstᵒ q 1)
         (restᵒ q [2 3]))
                    ⇒ '((1 2 3))

       (run* [q]
         (consᵒ 1 [2 3] q)
         (firstᵒ q 0))
                    ⇒ '()

       (run* [q]
         (consᵒ 1 [2 3] q)
         (restᵒ q [0]))
                    ⇒ '()

       (run* [x q]
         (consᵒ x [2 3] q)
         (firstᵒ q x))
                    ⇒ '(–₀ (–₀ 2 3))

       (run* [x q]
         (consᵒ 1 x q)
         (restᵒ q x))
                    ⇒ '(–₀ (1 . –₀))))

(deftest ^:mk memberᵒ-the-divergent
  (are [a _ e] (is (= a e))

       (run* [q]
         (memberᵒ q [1 2 3]))
                    ⇒ '(1 2 3)

       (run* [q]
         (memberᵒ 7 [1 3 8 q]))
                    ⇒ '(7)

       (run 3 [q]
         (memberᵒ 3 q))
                    ⇒ '((3 . –₀) (–₀ 3 . –₁) (–₀ –₁ 3 . –₂))))

;; Partly from http://objectcommando.com/blog/2011/10/13/appendo-the-great/
(deftest ^:mk appendᵒ-the-great
  (defn sublistᵒ [x y]
    (fresh [a b c]
      (appendᵒ a b y)
      (appendᵒ x c b)))

  (defn prefixᵒ [x y]
    (fresh [a]
      (appendᵒ x a y)))

  (defn lastᵒ [x y]
    (fresh [a]
      (appendᵒ a [x] y)))

  (are [a _ e] (is (= a e))

       (run* [q]
         (appendᵒ '(1 2 3) '(4 5) q))
                       ⇒ '((1 2 3 4 5))

       (run* [q]
         (appendᵒ [1 2] [3 4] q))
                    ⇒ '((1 2 3 4))

       (run* [q]
         (appendᵒ [1 2] q [1 2 3 4]))
                    ⇒ '((3 4))

       (run* [q]
         (appendᵒ q [3 4] [1 2 3 4]))
                       ⇒ '((1 2))

       (run* [w q]
         (appendᵒ w q [1 2 3 4]))
                       ⇒ '(() (1 2 3 4)
                           (1) (2 3 4)
                           (1 2) (3 4)
                           (1 2 3) (4)
                           (1 2 3 4) ())

       (run* [q]
         (prefixᵒ [1 2 3] q))
                       ⇒ '((1 2 3 . –₀))

       ;; StackOverflow
       ;; (run 3 [q]
       ;;   (lastᵒ "Hail to the king baby" q)
       ;;   (prefixᵒ [1 2 3] q))
       ;;                 ⇒ '((1 2 3 "Hail to the king baby")
       ;;                     (1 2 3 –₀ "Hail to the king baby")
       ;;                     (1 2 3 –₀ –₁ "Hail to the king baby"))

       ;; Only returns prefixed lists
       ;; (set (run* [q]
       ;;   (sublistᵒ q [1 2 3 4 5])))
       ;;                 ⇒ '#{() (1) (2) (1 2 3) (2 3) (3) (3 4) (4 5) (2 3 4)
       ;;                      (1 2 3 4 5) (1 2) (2 3 4 5) (4) (3 4 5) (1 2 3 4) (5)}
))

(deftest ^:mk anonymous-vars
  (is (= (run* [q] (≡ q _)) '(–₀))))

;; From https://github.com/swannodette/logic-tutorial
(deftest ^:mk zebra
  (defn rightᵒ [x y l]
    (condᵉ
     ((prefixᵒ [x y] l))
     ((fresh [d]
       (restᵒ l d)
       (rightᵒ x y d)))))

  (defn nextᵒ [x y l]
    (condᵉ
     ((rightᵒ x y l))
     ((rightᵒ y x l))))

  (are [a _ e] (is (= a e))

       (run* [q]
         (rightᵒ 1 2 [1 2]))
                       ⇒ '(–₀)

       (run* [q]
         (rightᵒ 1 2 [0 1 2 3]))
                       ⇒ '(–₀)

       (run* [q]
         (rightᵒ 1 2 [1]))
                       ⇒ '()

       (run* [q]
         (rightᵒ 1 2 [0 1 3 2 3]))
                       ⇒ '()

       (run* [q]
         (nextᵒ 1 2 [3 2 1]))
                       ⇒ '(–₀)

       (run* [q]
         (nextᵒ 1 2 [1 3 2]))
                       ⇒ '())

  ;; Doesn't run yet, small subsets "work".
  ;; (is (= (run 1 [hs]
  ;;             (≡ [_ _ [_ _ 'milk _ _] _ _] hs)
  ;;             (firstᵒ hs ['norwegian _ _ _ _])
  ;;             (nextᵒ ['norwegian _ _ _ _] [_ _ _ _ 'blue] hs)
  ;;             (rightᵒ [_ _ _ _ 'ivory] [_ _ _ _ 'green] hs)
  ;;             (memberᵒ ['englishman _ _ _ 'red] hs)
  ;;             (memberᵒ [_ 'kools _ _ 'yellow] hs)
  ;;             (memberᵒ ['spaniard _ _ 'dog _] hs)
  ;;             (memberᵒ [_ _ 'coffee _ 'green] hs)
  ;;             (memberᵒ ['ukrainian _ 'tea _ _] hs)
  ;;             (memberᵒ [_ 'lucky-strikes 'oj _ _] hs)
  ;;             (memberᵒ ['japanese 'parliaments _ _ _] hs)
  ;;             (memberᵒ [_ 'oldgolds _ 'snails _] hs)
  ;;             (nextᵒ [_ _ _ 'horse _] [_ 'kools _ _ _] hs)
  ;;             (nextᵒ [_ _ _ 'fox _] [_ 'chesterfields _ _ _] hs))

  ;;        '([[norwegian kools –₀ fox yellow]
  ;;           [ukrainian chesterfields tea horse blue]
  ;;           [englishman oldgolds milk snails red]
  ;;           [spaniard lucky-strikes oj dog ivory]
  ;;           [japanese parliaments coffee –₁ green]])))
)

(alter-var-root #'*match-var?* (constantly mv))
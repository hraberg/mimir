(ns mimir.mk
  (:use [clojure.tools.logging :only (debug info warn error spy)]
        [mimir.match :only (filter-walk prepare-matcher *match-var?* match-any bind-vars MatchAny MatchSeq)]
        [clojure.walk :only (postwalk-replace postwalk)])
  (:import [java.io Writer]
           [clojure.lang Symbol Seqable])
  (:refer-clojure :exclude [reify var? ==]))

;; mímirKanren: loosely based on "Implementation I: Core miniKanren", Chapter 3 in Byrd.

(defprotocol MatchVar (match-var [this x acc]))

(extend-protocol MatchVar
  Object
  (match-var [x this acc] (when-let [x (-> x meta :tag)]
                            (match-any x this acc)))
  nil
  (match-var [x this acc])
  Symbol
  (match-var [x this acc]))

(extend-protocol MatchAny
  Object
  (match-any [this x acc] (if (= this x) acc
                              (match-var x this acc)))
  Symbol
  (match-any [this x acc] (if (= this x) acc
                              (match-var x this acc)))
  nil
  (match-any [this x acc] (if (nil? x) acc
                              (match-var x this acc))))

(deftype LVar [name]
  MatchAny
  (match-any [this x acc] (if (= this x) acc
                              (bind-vars x this acc)))
  MatchVar
  (match-var [x this acc] (match-any x this acc))
  MatchSeq
  (match-seq [x this acc] (when ((every-pred sequential? seq) (acc x))
                            (match-any this (acc x) acc)))
  Object
  (hashCode [this] (if name (.hashCode name) 0))
  (equals [this o] (and (instance? LVar o) (= (.name this) (.name ^LVar o)))))

(defmethod print-method LVar [o ^Writer w]
  (.write w (str (.name o))))

(defmacro alias-macro [m a]
  `(doto (intern *ns* '~a (var ~m)) .setMacro))

(defn var? [x] (instance? LVar x))

(defn cons-pairs-to-seqs [x]
  (if (and (sequential? x) (= 3 (count x)) (= '. (second x))
           ((some-fn sequential? nil?) (last x)))
    (cons (first x) (last x))
    x))

(defmacro unify [u v s]
  (let [[u v] (map #(prepare-matcher % &env) [u v])]
    `(binding [*match-var?* var?]
       (merge (match-any ~u ~v ~s) (match-any ~v ~u ~s)))))

(def ^:private subscripts '[₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉])

(defn reify-name [n]
  (symbol (apply str "–" (map (comp subscripts int bigdec str) (str n)))))

(defn reify [v s]
  (loop [v v s s check #{v}]
    (let [v' (postwalk-replace s v)]
      (debug v')
      (if (contains? check v')
        v'
        (recur v' s (conj check v'))))))

(defmacro ≡ [u v]
  `(fn ≡ [a#]
     [(unify ~u ~v a#)]))
(alias-macro ≡ ==)

(defmacro ≠ [u v]
  `(fn ≠ [a#]
     [(when-not (seq (select-keys (unify ~u ~v a#) (keys a#))) a#)]))
(alias-macro ≠ !=)

(defn interleave-all [& colls]
  (when-let [ss (seq (remove nil? (map seq colls)))]
    (concat (map first ss) (lazy-seq (apply interleave-all (map rest ss))))))

(defmacro condᵉ [& gs]
  (let [a (gensym "a")]
    `(fn condᵉ [~a]
       (interleave-all ~@(map #(do `(run-internal ~(vec %) [~a])) gs)))))
(alias-macro condᵉ conde)

(defmacro fresh [[& x] & gs]
  `(let [~@(mapcat (fn [x] `[~x (LVar. (gensym '~x))]) x)]
     [~@gs]))

(defmacro project [[& x] & gs]
  (let [a (gensym "a")]
    `(fn project [~a]
       (let [~@(mapcat (fn [x] `[~x (~a ~x)]) x)]
         (run-internal ~(vec gs) [~a])))))

(defn run-internal [gs s]
  (lazy-seq
    (let [[g & gs] (flatten gs)
          s (remove nil? s)]
      (if-not g
        s
        (mapcat #(when-let [s (g %)]
                   (concat (run-internal gs [(first s)])
                           (run-internal gs (rest s)))) s)))))

(defn reify-goal [xs s]
  (let [xs (map #(reify % s) xs)
        vs (loop [[v & vs] (distinct (filter-walk var? xs))
                  acc {}]
             (if-not v
               acc
               (recur vs (assoc acc v (or (acc (s v)) (reify-name (count acc)))))))]
    (postwalk cons-pairs-to-seqs (postwalk-replace vs xs))))

(defmacro run* [[& x] & g]
  (let [g (postwalk-replace {'_ '(mimir.mk.LVar. (gensym '_))} g)]
    `(binding [*match-var?* var?]
       (run-internal (fresh [~@x] ~@g (partial reify-goal ~(vec x))) [{}]))))

(defmacro run [n [& x] & g]
  `(take ~n (run* [~@x] ~@g)))

(def succeed (≡ false false))
(def fail (≡ false true))

(defn consᵒ [a d l]
  (if (var? l)
    (let [d (if (var? d) ['. d] d)]
      (≡ (cons a d) l))
    [(≡ a (first l))
     (≡ d (rest l))]))

(defn firstᵒ [l a]
  (fresh [d]
    (consᵒ a d l)))

(defn restᵒ [l d]
  (fresh [a]
    (consᵒ a d l)))

(defn memberᵒ [x ls]
  (fresh [a d]
    (consᵒ a d ls)
    (condᵉ
      ((≡ a x))
      ((memberᵒ x d)))))

(defn appendᵒ [l1 l2 o]
  (condᵉ
   ((≡ l1 ()) (≡ l2 o))
   ((fresh [a d r]
      (consᵒ a d l1)
      (consᵒ a r o)
      (appendᵒ d l2 r)))))

(ns mimir.mk
  (:use [clojure.tools.logging :only (debug info warn error spy)]
        [mimir.match :only (filter-walk prepare-matcher *match-var?* match-any bind-vars MatchAny MatchSeq)]
        [clojure.walk :only (postwalk-replace postwalk)])
  (:import [java.io Writer]
           [clojure.lang Symbol Seqable])
  (:refer-clojure :exclude [reify var? ==]))

;; Loosely based on "Implementation I: Core miniKanren", Chapter 3 in Byrd.
;; Uses Mímir's matcher to unify.

(defprotocol MatchVar (match-var [this x acc]))

(extend-protocol MatchVar
  Object
  (match-var [x this acc] (when-let [x (-> x meta :tag)]
                            (match-any x this acc)))
  nil
  (match-var [this x acc])
  Symbol
  (match-var [x this acc] (match-any x this acc)))

(extend-protocol MatchAny
  Object
  (match-any [this x acc] (if (= this x) acc
                              (match-var x this acc)))
  nil
  (match-any [this x acc] (if (nil? x) acc
                                       (match-var x this acc))))

(deftype LVar [name]
  MatchAny
  (match-any [this x acc] (bind-vars x this acc))
  MatchVar
  (match-var [x this acc] (match-any x this acc))
  MatchSeq
  (match-seq [x this acc] (match-any this (acc x) acc))
  Object
  (hashCode [this] (if name (.hashCode name) 0))
  (equals [this o] (and (instance? LVar o) (= (.name this) (.name o)))))

(defmethod print-method LVar [o ^Writer w]
  (.write w (str (.name o))))

(defmacro alias-macro [m a]
  `(doto (intern *ns* '~a (var ~m)) .setMacro))

(defn var? [x] (instance? LVar x))

(defn cons-pairs-to-seqs [x]
  (if (and (sequential? x) (= 3 (count x)) (= '. (second x)))
    (cond
      ((some-fn sequential? nil?) (last x)) (cons (first x) (last x))
      ((some-fn sequential? nil?) (first x)) (concat (first x) (rest x))
      :else x)
    x))

(defmacro unify [u v s]
  `(binding [*match-var?* var?]
     (let [u# (match-any ~(prepare-matcher u &env) ~(prepare-matcher v &env) ~s)
           v# (match-any ~(prepare-matcher v &env) ~(prepare-matcher u &env) ~s)]
       (println "UNI" '~u '~v ~s u# v# (merge u# v#))
       (merge u# v#))))

(def ^:private subscripts '[₀ ₁ ₂ ₃ ₄ ₅ ₆ ₇ ₈ ₉])

(defn reify-name [n]
  (symbol (apply str "–" (map (comp subscripts int bigdec str) (str n)))))

(defn reify [v s]
  (let [v' (postwalk-replace s v)]
    (if (or (= v v') (= v (s v')))
      v'
      (recur v' s))))

(defmacro ≡ [u v]
  `(fn [a#]
     [(unify ~u ~v a#)]))
(alias-macro ≡ ==)

(defmacro ≠ [u v]
  `(fn [a#]
     [(when-not (seq (select-keys (unify ~u ~v a#) (keys a#))) a#)]))
(alias-macro ≠ !=)

(defn interleave-all [& colls]
  (lazy-seq
    (when-let [ss (seq (remove nil? (map seq colls)))]
      (concat (map first ss) (apply interleave-all (map rest ss))))))

(defmacro condᵉ [& gs]
  (let [a (gensym "a")]
    `(fn [~a] (interleave-all ~@(map #(do `(run-internal ~(vec %) [~a])) gs)))))
(alias-macro condᵉ conde)

(defmacro exist [[& x] & gs]
  `(let [~@(mapcat (fn [x] `[~x (LVar. (gensym '~x))]) x)]
     [~@gs]))
(alias-macro exist fresh)

(defn run-internal [[g & gs] s]
  (println "S" s)
  (if (sequential? g)
    (run-internal (concat g gs) s)
    (if-not g
      s
      (if-let [s (seq (remove nil? (mapcat #(g %) s)))]
        (recur gs s)
        ()))))

(defn reify-goal [xs s]
  (let [xs (map #(reify % s) xs)
        vs (distinct (filter-walk var? xs))
        vs (zipmap vs (map-indexed (fn [idx _] (reify-name idx)) vs))]
    (postwalk cons-pairs-to-seqs (postwalk-replace vs xs))))

(defmacro run* [[& x] & g]
  `(binding [*match-var?* var?]
     (run-internal (exist [~@x] ~@g (partial reify-goal ~(vec x))) [{}])))

(defmacro run [n [& x] & g]
  `(take ~n (run* [~@x] ~@g)))

(def succeed (≡ false false))
(def fail (≡ false true))

(defn consᵒ [a d l]
  (println "CONSO" a d l)
  (if (var? l)
    (let [d (if (var? d) ['. d] d)]
      [(≠ () l)
       (≡ (cons a d) l)])
    [(≡ a (first l))
     (≡ d (rest l))]))

(defn firstᵒ [l a]
  (println "FIRSTO" l a)
  (fresh [d]
    (consᵒ a d l)))

(defn restᵒ [l d]
  (fresh [a]
    (consᵒ a d l)))

; these doesn't work, LVar seq hack is too simplistic
(defn memberᵒ [x ls]
  (println "MEMBERO" x ls)
  (fresh [a d]
    (consᵒ a d ls)
    (conde
      ((≡ a x))
      ((memberᵒ x d)))))

(defn appendᵒ [l1 l2 o]
  (condᵉ
   ((≡ l1 ()) (≡ l2 o))
   ((fresh [a d r]
      (consᵒ a d l1)
      (consᵒ a r o)
      (appendᵒ d l2 r)))))

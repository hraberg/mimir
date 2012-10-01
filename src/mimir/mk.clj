(ns mimir.mk
  (:use [clojure.tools.logging :only (debug info warn error spy)]
        [mimir.match :only (filter-walk prepare-matcher *match-var?* match-any bind-vars MatchAny)]
        [clojure.walk :only (postwalk-replace)])
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

  ;; Seqable ; hack for consᵒ
  ;; (seq [this] (seq ['& this]))

  Object
  (hashCode [this] (.hashCode name))
  (equals [this o] (and (instance? LVar o) (= (.name this) (.name o)))))

(defmethod print-method LVar [o ^Writer w]
  (.write w (str "(LVar. '" (.name o) ")")))

(defmacro alias-macro [m a]
  `(doto (intern *ns* '~a (var ~m)) .setMacro))

(defn var? [x] (instance? LVar x))
(alter-var-root #'*match-var?* (constantly var?))

(defmacro unify [u v s]
  `(let [u# (match-any ~(prepare-matcher u &env) ~(prepare-matcher v &env) ~s)
         v# (match-any ~(prepare-matcher v &env) ~(prepare-matcher u &env) ~s)]
     (merge u# v#)))

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
     [(when-not (unify ~u ~v a#) a#)]))
(alias-macro ≠ !=)

(defmacro condᵉ [& gs]
  (let [a (gensym "a")]
    `(fn [~a] (concat ~@(map #(do `(run-internal ~(vec %) [~a])) gs)))))
(alias-macro condᵉ conde)

(defmacro exist [[& x] & gs]
  `(let [~@(mapcat (fn [x] [x (LVar. x)]) x)]
     [~@gs]))
(alias-macro exist fresh)

(defn ^:private run-internal [[g & gs] s]
  (if (sequential? g)
    (run-internal (concat g gs) s)
    (if-not g
      s
      (if-let [s (seq (remove nil? (mapcat #(g %) s)))]
        (recur gs s)
        ()))))

(defn ^:private reify-goal [xs s]
  (let [xs (map #(reify % s) xs)
        vs (distinct (filter-walk var? xs))
        vs (zipmap vs (map-indexed (fn [idx _] (reify-name idx)) vs))]
    (postwalk-replace vs xs)))

(defmacro run* [[& x] & g]
  `(run-internal (exist [~@x] ~@g (partial reify-goal ~(vec x))) [{}]))

(defmacro run [n [& x] & g]
  `(take ~n (run* [~@x] ~@g)))

(def succeed (≡ false false))
(def fail (≡ false true))

(defn consᵒ [a d l]
  (let [d (if (var? d) ['& d] d)]
    (≡ l (cons a d))))

(defn firstᵒ [l a]
  (fresh [d]
    (consᵒ a d l)))

(defn restᵒ [l d]
  (fresh [a]
    (consᵒ a d l)))

(defn memberᵒ [x l]
  [(≠ l ())
   (condᵉ
    ((firstᵒ l x))
    ((memberᵒ x (rest l))))])

; doesn't work, LVar seq hack is too simplistic
(defn appendᵒ [l1 l2 o]
  (condᵉ
   ((≡ l1 ()) (≡ l2 o))
   ((fresh [a d r]
           (consᵒ a d l1)
           (consᵒ a r o)
           (appendᵒ d l2 r)))))

(ns mimir.match
  (:use [clojure.set :only (intersection map-invert rename-keys difference union join)]
        [clojure.tools.logging :only (debug info warn error spy enabled?)]
        [clojure.walk :only (postwalk prewalk walk postwalk-replace)])
  (:import [java.util.regex Pattern]
           [clojure.lang IPersistentMap IPersistentSet Sequential Symbol Fn Keyword]))

(defprotocol MatchAny (match-any [this x acc]))
(defprotocol MatchMap (match-map [this x acc]))
(defprotocol MatchSeq (match-seq [this x acc]))

(defn filter-walk
  [pred coll]
  (let [acc (transient [])]
    (postwalk #(when (pred %) (conj! acc %)) coll)
    (distinct (persistent! acc))))

(defn singleton-coll? [x]
  (and (coll? (first x)) (not (next x))))

(defn maybe-singleton-coll [x]
  (if (singleton-coll? x) (first x) x))

(def default-match-var? #(and (symbol? %)
                                  (not (or (resolve %)  ('#{do fn* let* if} %)
                                           (re-matches #".*/.*"(str %)) (re-matches #"\..*"(name %))
                                           (re-matches #".*\."(name %)) (re-matches #".*#"(name %))))))
(def ^:dynamic *match-var?* default-match-var?)

(def ^:dynamic *var-symbol* symbol)

(defn bind-vars [x pattern acc]
  (if-let [var (if (*match-var?* pattern)
                 pattern
                 (-> pattern meta :tag))]
    (if (contains? acc var)
      (let [v (acc var)]
        (if-not (= v var)
          (if (= (acc v) var)
            (assoc acc var x)
            (match-any v x acc))
          acc))
      (assoc acc var x))
    acc))

(defn preserve-meta [form meta]
  (if (and (instance? clojure.lang.IMeta form)
           (not (and (list? form)
                     (= 'quote (first form))
                     (symbol (second form)))))
    (list 'if (list 'instance? 'clojure.lang.IMeta form)
          (list 'with-meta form (list 'quote meta))
          form)
    form))

(defn meta-walk [form]
  (let [m (dissoc (meta form) :line)]
    (if (seq m)
      (preserve-meta (walk meta-walk identity form) m)
      (if (*match-var?* form)
        (list 'quote form)
        (walk meta-walk identity form)))))

(defn bound-vars [x]
  (let [vars (transient [])
        var-walk (fn this [form]
                   (let [v (or (-> form meta :tag) form)]
                     (when (*match-var?* v)
                       (conj! vars v)))
                   form)]
    (prewalk var-walk x)
    (distinct (persistent! vars))))

(defn regex-vars [x]
  (let [vars (transient [])
        regex-walk (fn this [form]
                     (when (instance? Pattern form)
                       (reduce conj! vars
                               (map (comp symbol second)
                                    (re-seq #"\(\?<(.+?)>.*?\)" (str form)))))
                     form)]
    (postwalk regex-walk x)
    (distinct (persistent! vars))))

(extend-type Object
  MatchAny (match-any [this x acc] (when (= this x) acc))
  MatchMap (match-map [this x acc])
  MatchSeq (match-seq [this x acc]))

(extend-type nil
  MatchAny (match-any [this x acc] (when (nil? x) acc))
  MatchMap (match-map [this x acc])
  MatchSeq (match-seq [this x acc] (when-not (seq x) acc)))

(extend-type IPersistentMap
  MatchAny
  (match-any [this x acc] (match-map x this acc))
  MatchMap
  (match-map [x this acc] (loop [[k & ks] (keys this)
                                 acc acc]
                            (if-not k
                              (bind-vars x this acc)
                              (when (contains? x k)
                                (when-let [acc (match-any (this k) (x k) acc)]
                                  (recur ks (bind-vars (x k) (this k) acc))))))))

(extend-type Symbol
  MatchAny
  (match-any [this x acc] (if (*match-var?* this)
                            (bind-vars x this acc)
                            (when (= this x) acc))))

(extend-type Pattern
  MatchAny
  (match-any [this x acc] (let [re (re-matcher this (str x))
                                groups (regex-vars this)]
                            (when (.matches re)
                              (reduce #(assoc % (*var-symbol* %2)
                                              (.group re (str %2)))
                                      acc groups)))))

(extend-type Class
  MatchAny
  (match-any [this x acc] (when (instance? this x) acc)))

(extend-type Fn
  MatchAny
  (match-any [this x acc] (when (try (this x) (catch RuntimeException _))
                            (bind-vars x this acc))))

(extend-type Keyword
  MatchAny
  (match-any [this x acc] (when (or (and (coll? x)
                                         (contains? x this))
                                    (= x this))
                            (bind-vars x this acc)))

  MatchMap
  (match-map [this x acc] (when (contains? x this)
                            (bind-vars x this acc))))

(extend-type IPersistentSet
  MatchAny
  (match-any [this x acc] (loop [[k & ks] (seq this)
                                 acc acc]
                            (when k
                              (if-let [acc (match-any k x acc)]
                                (bind-vars x this acc)
                                (recur ks acc))))))

(def rest? '#{& .})

(extend-type Sequential
  MatchAny
  (match-any [this x acc] (match-seq x this acc))
  MatchSeq
  (match-seq [x this acc] (loop [[p & ps] this
                                 [y & ys] x
                                 acc acc]
                            (if (rest? y)
                              (when (rest? p) (recur ps ys acc))
                              (if (and (not p) (not y))
                                (bind-vars x this acc)
                                (if (rest? p)
                                  (let [rst (when y (vec (cons y ys)))]
                                    (when-let [acc (if (*match-var?* (first ps))
                                                     acc
                                                     (match-seq rst (repeat (count rst)
                                                                            (first ps)) acc))]
                                      (bind-vars (or rst ()) (first ps) acc)))
                                  (when-let [acc (match-any p y acc)]
                                    (recur ps ys (bind-vars y p acc)))))))))

(defn truth [& _] true)

(defn unquote-vars-in-scope [&env form]
  (if &env
    (postwalk #(if (and (list? %)
                        (= 'quote (first %))
                        (&env (second %)))
                 (second %) %) form)
    form))

(defn prepare-matcher [m &env]
  (->> (preserve-meta (walk identity meta-walk m) (meta m))
       (postwalk-replace {'_ truth :else truth '. ''.})
       (unquote-vars-in-scope &env)))

(defn match* [x pattern] (match-any pattern x {}))

(defmacro match [x m]
  `(match* ~x ~(prepare-matcher m &env)))

(defn all-vars [lhs]
  (vec (concat (bound-vars lhs)
               (map *var-symbol* (regex-vars lhs)))))

(defmacro condm* [match-var [lhs rhs & ms]]
  `(if-let [{:syms ~(remove (set (keys &env)) (all-vars lhs))}
            (mimir.match/match ~match-var ~lhs)]
     ~rhs
     ~(when ms
        `(condm* ~match-var ~ms))))

(defmacro condm [x & ms]
  (let [match-var (if-let [v (-> x meta :tag)] v '*match*)]
    `(let [~match-var ~(if (and (instance? clojure.lang.IMeta x)
                                (not (and (list? x)
                                          (= 'quote (first x))
                                          (symbol? (second x)))))
                         (with-meta x {})
                         x)]
       (condm* ~match-var ~ms))))

(defn single-arg? [ms]
  (not-any? coll? (take-nth 2 ms)))

(defmacro fm [& ms]
  `(fn ~'this [& ~'args]
     (condm (if ~(single-arg? ms) (first ~'args) ~'args) ~@ms)))

(defmacro defm [name args & ms]
  (let [[doc ms] (split-with string? ms)
        [_ _ [match-var & _ ]] (partition-by '#{&} args)]
    `(do
       (defn ~name ~args
         ~(when (seq ms)
            `(condm ~(list 'first (if (single-arg? ms)
                                    (list 'first match-var)
                                    match-var)) ~@ms)))
       (alter-meta! (var ~name) merge {:doc (apply str ~doc)})
       ~name)))

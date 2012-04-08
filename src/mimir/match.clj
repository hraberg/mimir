(ns mimir.match
  (:use [clojure.set :only (intersection map-invert rename-keys difference union join)]
        [clojure.tools.logging :only (debug info warn error spy enabled?)]
        [clojure.walk :only (postwalk prewalk walk postwalk-replace)])
  (:import [java.util.regex Pattern]))

(defn filter-walk
  [pred coll]
  (let [acc (transient [])]
    (postwalk #(when (pred %) (conj! acc %)) coll)
    (distinct (persistent! acc))))

(defn singleton-coll? [x]
  (and (= 1 (count x))
       (coll? (first x))))

(def ^:dynamic *match-var?* #(and (symbol? %) (not (resolve %))))

(def ^:dynamic *var-symbol* symbol)

(defn bind-vars [x pattern acc]
  (if-let [var (if (*match-var?* pattern)
                 pattern
                 (-> pattern meta :tag))]
    (assoc acc var x)
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
  (if-let [m (meta form)]
    (preserve-meta (walk meta-walk identity form) m)
    (if (*match-var?* form)
      (list 'quote form)
      (walk meta-walk identity form))))

(defn bound-vars [x]
  (let [vars (transient [])
        var-walk (fn this [form]
                   (when-let [v (-> form meta :tag)]
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

(defn match*
  ([x pattern] (match* x pattern {}))
  ([x pattern acc]
     (condp some [pattern]
       *match-var?* (assoc acc pattern x)
       (partial
        instance?
        Pattern) (let [re (re-matcher pattern (str x))
                       groups (regex-vars pattern)]
                   (when (.matches re)
                     (reduce #(assoc % (*var-symbol* %2)
                                     (.group re (str %2)))
                             acc groups)))
        (partial
         instance?
         Class) (when (instance? pattern x)
                  acc)
         fn? (when (try (pattern x) (catch RuntimeException _))
               (bind-vars x pattern acc))
         set? (loop [[k & ks] (seq pattern)
                     acc acc]
                (when k
                  (if-let [acc (match* x k acc)]
                    (bind-vars x pattern acc)
                    (recur ks acc))))
         map? (when (map? x)
                (loop [[k & ks] (keys pattern)
                       acc acc]
                  (if-not k
                    (bind-vars x pattern acc)
                    (when-let [acc (match* (x k) (pattern k) acc)]
                      (recur ks (bind-vars (x k) (pattern k) acc))))))
         sequential? (when (sequential? x)
                       (loop [[p & ps] pattern
                              [y & ys] x
                              acc acc]
                         (if-not p
                           (bind-vars x pattern acc)
                           (if (= '& p)
                             (let [rst (when y (vec (cons y ys)))]
                               (when-let [acc (if (*match-var?* (first ps))
                                                acc
                                                (match* rst (repeat (count rst)
                                                                    (first ps)) acc))]
                                 (bind-vars rst (first ps) acc)))
                             (when-let [acc (match* y p acc)]
                               (recur ps ys (bind-vars y p acc)))))))
         #{x} acc
         nil)))

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
       (postwalk-replace {'_ truth :else truth})
       (unquote-vars-in-scope &env)))

(defmacro match [x m]
  `(match* ~x ~(prepare-matcher m &env)))

(defn all-vars [lhs]
  (vec (concat (filter-walk *match-var?* lhs)
               (bound-vars lhs)
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

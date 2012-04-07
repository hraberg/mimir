(ns mimir.well
  (:use [clojure.set :only (intersection map-invert rename-keys difference union join)]
        [clojure.tools.logging :only (debug info warn error spy)]
        [clojure.walk :only (postwalk prewalk walk postwalk-replace macroexpand-all)])
  (:refer-clojure :exclude [assert])
  (:import [java.util.regex Pattern])
  (:gen-class))

(defn create-net []
  {:productions #{}
   :working-memory #{}
   :predicates {}
   :predicate-invokers {}
   :alpha-network {}
   :beta-join-nodes {}})

(def ^:dynamic *net* (atom (create-net)))

(defn working-memory [] (:working-memory @*net*))

(defn alpha-network [] (:alpha-network @*net*))

(defn beta-join-nodes [] (:beta-join-nodes @*net*))

(defn triplet? [x]
  (and (sequential? x) (= 3 (count x)) (symbol? (second x))))

(defn is-var? [x]
  (if (symbol? x)
    (let [s (name x)]
      (or (.startsWith s "?")
          (re-matches #"[A-Z]+" s)))
    false))

(defn is-matcher? [x xs]
  (and (is-var? x) (not (symbol? (first xs)))))

(defn parser
  ([x] (parser x identity identity))
  ([[x & xs] atom-fn triplet-fn]
     (when x
       (cond ((some-fn
               sequential? map? set? string?) x) (cons (atom-fn x)
                                                       (parser xs atom-fn triplet-fn))
               (is-matcher? x xs) (cons (atom-fn (list 'mimir.well/match* x (first xs)))
                                        (parser (rest xs) atom-fn triplet-fn))
               :else (cons (triplet-fn (cons x (take 2 xs)))
                           (parser (drop 2 xs) atom-fn triplet-fn))))))

(defn quote-non-vars [rhs]
  (postwalk #(if (and (symbol? %)
                      (not (is-var? %))) (list 'quote %) %) rhs))

(defn vars [x]
  (let [vars (transient [])]
    (postwalk #(when (is-var? %) (conj! vars %)) x)
    (distinct (persistent! vars))))

(defn quote-fact [t]
  (list 'quote t))

(defn expand-rhs [t]
  (cons 'mimir.well/assert t))

(def relations (reduce (fn [m rel] (assoc m rel rel))
                       '{<- mimir.well/match* = mimir.well/match* != not=} '[< > <= => not=]))

(defn expand-lhs [t]
  (if-let [rel (relations (second t))]
    (let [[var _ & [rest]] t]
      (list rel var rest))
    t))

(defn ellipsis
  ([x] (ellipsis 5 x))
  ([n x]
     (let [[start more] (split-at n (take (inc n) x))]
       (str (seq start)
            (when more
              (str "... [total: " (count x)
                   "]"))))))

(defn macroexpand-conditions [lhs]
  (loop [[c & cs] (map macroexpand lhs)
         acc []]
    (if-not c
      acc
      (recur cs
             (if (every? seq? c)
               (into acc c)
               (conj acc c))))))

(defmacro rule [name & body]
  (let [[lhs _ rhs] (partition-by '#{=>} body)
        [doc lhs] (split-with string? lhs)
        expanded-lhs (macroexpand-conditions (parser lhs expand-lhs expand-lhs))
        rhs (parser rhs identity expand-rhs)]
    `(let [f# (defn ~name
                ([] (~name {}))
                ([~'args] (map #(%) (~name (working-memory) ~'args)))
                ([~'wm ~'args]
                   (debug "rule" '~name '~*ns*)
                   (doall
                    (for [vars# (binding [*ns* '~*ns*]
                                  (check-rule '~(vec expanded-lhs) ~'wm ~'args))
                          :let [{:syms ~(vars rhs)} vars#
                                ~'*vars* (map val (sort-by key vars#))]]
                      #(do
                         (debug "rhs" vars#)
                         ~@rhs)))))]
       (debug "defining rule" '~name)
       (when-not (= '~lhs '~expanded-lhs)
         (debug "expanded" '~lhs)
         (debug "    into" '~expanded-lhs))
       (alter-meta! f# merge {:lhs '~lhs :rhs '~rhs :doc ~(apply str doc)})
       (swap! *net* update-in [:productions] conj f#)
       f#)))

(defmacro with-cache [cache-name key & f]
  (let [cache-name (keyword cache-name)]
    `(let [key# ~key]
       (if-not (contains? ('~cache-name @*net*) key#)
         (let [v# (do ~@f)]
           (swap! *net* assoc-in ['~cache-name key#] v#)
           v#)
         (get-in @*net* ['~cache-name key#])))))

(defn join-on [x y]
  (intersection (set (vars x)) (set (vars y))))

(defn var-sym [x]
  (symbol (str "?" x)))

(defn var-to-index [c]
  (loop [[v & vs] (vars c)
         acc {}]
    (if v
      (recur vs (if (acc v)
                  acc
                  (assoc acc v (var-sym (inc (count acc))))))
      acc)))

(defn ordered-vars [c]
  (->> (var-to-index c) vals sort vec))

(defn predicate-for [c]
  (with-cache predicate c
    (let [args (ordered-vars c)
          src `(fn ~args ~c)]
      (debug " compiling" c)
      (with-meta (eval src) {:src src :args args}))))

(defn match-using-predicate [c wme]
  (let [predicate (predicate-for c)]
    (try
      (when (predicate wme)
        (debug " evaluated to true" wme)
        {'?1 wme})
      (catch RuntimeException e
        (debug " threw non fatal" e)))))

(defn match-triplet [c wme]
  (loop [[v & vs] wme [t & ts] c m {}]
    (if v
      (condp some [t]
        #{v} (recur vs ts m)
        is-var? (recur vs ts (assoc m t v))
        nil)
      (do
        (debug " evaluated to true" wme)
        m))))

(defn predicate? [c]
  (-> c first
      ((some-fn
        (every-pred
         symbol? resolve)
        (every-pred
         (complement symbol?) ifn?)))))

(defn multi-var-predicate? [c]
  (and (predicate? c) (> (count (vars c)) 1)))

(defn multi-var-predicate-placeholder [c]
  (let [pred (predicate-for c)]
    (debug " more than one argument, needs beta network")
    (with-meta (zipmap (-> pred meta :args) (repeat pred)) (meta pred))))

(defn match-wme [c wme]
  (if (predicate? c)
    (match-using-predicate c wme)
    (match-triplet c wme)))

(defn ^:private wm-crud [action test msg fact]
  (when (test (working-memory) fact)
    (debug msg " fact" fact)
    (swap! *net* update-in [:working-memory] action fact)
    (doseq [c (keys (:alpha-network @*net*))
            :let [match (match-wme c fact)]
            :when match]
      (debug " alpha network change" match)
      (swap! *net* update-in [:alpha-network] #(merge-with action % {c match}))))
  fact)

(defn fact [fact]
  (wm-crud conj (complement contains?) "asserting" fact))

(defn retract* [fact]
  (wm-crud disj contains? "retracting" fact))

(defmacro facts [& wms]
  (when wms
    `(doall
      (for [wm# ~(vec (parser wms identity quote-fact))]
        (fact wm#)))))

(defn matching-wmes
  ([c] (matching-wmes c (working-memory)))
  ([c wm]
     (debug "condition" c)
     (if (multi-var-predicate? c)
       #{(multi-var-predicate-placeholder c)}
       (->> wm
            (map #(match-wme c %))
            (remove nil?)
            (set)))))

(defn alpha-network-lookup [c wm]
  (with-cache alpha-network c
    (matching-wmes c wm)))

(defn alpha-memory
  ([c] (alpha-memory c (working-memory)))
  ([c wm]
     (let [var-to-index (var-to-index c)
           vars-by-index (map-invert var-to-index)]
       (->> (alpha-network-lookup (postwalk-replace var-to-index c) wm)
            (map #(rename-keys (with-meta % (postwalk-replace vars-by-index (meta %))) vars-by-index))))))

(defn cross [left right]
  (debug " nothing to join on, treating as or")
  (into #{}
        (for [x left y right]
          (merge x y))))

(defn multi-var-predicate-node? [am]
  (if (and (seq? am) (= 1 (count am)))
    (fn? (-> am first first val))
    false))

(defn permutations
  ([coll] (permutations (count coll) coll))
  ([n coll]
     (if (zero? n)
       '(())
       (for [x (permutations (dec n) coll) y coll]
         (conj x y)))))

(defn predicate-invoker [args join-on]
  (with-cache predicate-invokers [args join-on]
    (eval `(fn [pred# {:syms [~@(filter join-on args)]} [~@(remove join-on args)]]
             (pred# ~@args)))))

(defn deal-with-multi-var-predicates [c1-am c2-am join-on]
  (let [pred (-> c2-am first first val)
        args (-> c2-am first meta :args)
        needed-args (remove join-on args)
        permutated-wm (permutations (count needed-args) (working-memory))
        invoker (predicate-invoker args join-on)]
    (debug " multi-var-predicate")
    (debug " args" args)
    (debug " known args" join-on "- need to find" needed-args)
    (debug " permutations of wm" (ellipsis permutated-wm))
    (mapcat
     (fn [m]
       (for [wmes permutated-wm
             :when (try
                     (invoker pred m wmes)
                     (catch RuntimeException e
                       (debug " threw non fatal" e)))]
         (merge m (zipmap needed-args wmes))))
     c1-am)))

(defn beta-join-node [c1 c2 c1-am wm]
  (let [c2-am (alpha-memory c2 wm)]
    (with-cache beta-join-nodes [c1-am c2-am]
      (let [join-on (join-on (-> c1-am first keys) c2)]
        (debug "join" join-on)
        (debug "  left" (ellipsis c1-am))
        (debug " right" (ellipsis c2-am))
        (let [result (cond
                      (multi-var-predicate-node? c2-am) (deal-with-multi-var-predicates c1-am c2-am join-on)
                      (empty? join-on) (cross c1-am c2-am)
                      :else (join c1-am c2-am))]
          (debug "result" (ellipsis result))
          result)))))

(defn dummy-beta-join-node [c wm args]
  (beta-join-node '() c #{args} wm))

(defn order-conditions [cs]
  (sort-by (comp count vars) cs))

(defn check-rule
  ([cs wm] (check-rule cs wm {}))
  ([cs wm args]
     (debug "conditions" cs)
     (loop [[c1 & cs] (order-conditions cs)
            matches (dummy-beta-join-node c1 wm args)]
       (if-not cs
         matches
         (let [c2 (first cs)]
           (recur cs (beta-join-node c1 c2 matches wm)))))))

(defn run-once [wm productions]
  (->> productions
       (mapcat #(% wm {}))
       (map #(%))
       set))

(defn run
  ([] (run *net*))
  ([net]
     (binding [*net* net]
       (loop [wm (working-memory)
              productions (:productions @net)
              acc #{}]
         (let [acc (union (run-once wm productions) acc)]
           (if (seq (difference (working-memory) wm))
             (recur (working-memory) productions acc)
             acc))))))

(defn reset []
  (reset! *net* (create-net)))

; rule writing fns

(defmacro assert
  ([fact]
     `(let [fact# (list ~@(quote-non-vars fact))]
        (fact fact#)))
  ([id rel attr]
     `(assert ~(list id rel attr))))

(defmacro retract
  ([fact]
     `(let [fact# (list ~@(quote-non-vars fact))]
        (retract* fact#)))
  ([id rel attr]
     `(retract ~(list id rel attr))))

(defn all-different [& xs]
  (apply distinct? xs))

(defn different [f & xs]
  (apply distinct? (map f xs)))

(defn same*
  ([test pred xs]
     (test (for [x xs y (remove #{x} xs)]
             (pred x y)))))

(defn not-same [pred & xs]
  (same* (partial not-any? true?) pred xs))

(defn same [pred & xs]
  (same* (partial every? true?) pred xs))

(defmacro unique [xs]
  (for [[x y] (partition 2 1 xs)]
    `(pos? (compare ~x ~y))))

(defn not-in [set]
  (complement set))

(defn is-not [x]
  (partial not= x))

; pattern matching

(defn bind-vars [x pattern acc]
  (if-let [var (if (is-var? pattern)
                 pattern
                 (-> pattern meta :tag))]
    (assoc acc var x)
    acc))

(defn meta-walk [form]
  (if-let [m (meta form)]
    (list 'with-meta (walk meta-walk identity form)
          (list 'quote m))
    (if (is-var? form)
      (list 'quote form)
      (walk meta-walk identity form))))

(defn bound-vars [x]
  (let [vars (transient [])
        var-walk (fn this [form]
                   (when-let [v (-> form meta :tag)]
                     (when (is-var? v)
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
       is-var? (assoc acc pattern x)
       (partial
        instance?
        Pattern) (let [re (re-matcher pattern (str x))
                       groups (regex-vars pattern)]
                   (when (.matches re)
                     (reduce #(assoc % (var-sym %2)
                                     (.group re (str %2)))
                             acc groups)))
        (partial
         instance?
         Class) (when (instance? pattern x)
                  acc)
         fn? (when (pattern x)
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
                             (when-let [rst (when y (vec (cons y ys)))]
                               (when-let [acc (match* rst (repeat (count rst)
                                                                  (first ps)) acc)]
                                 (bind-vars rst (first ps) acc)))
                             (when-let [acc (match* y p acc)]
                               (recur ps ys (bind-vars y p acc)))))))
         #{x} acc
         nil)))

(defmacro match [x m]
  `(match* ~x ~(postwalk-replace
                {'_ identity '& (list 'quote '&)}
                (list 'with-meta (walk identity meta-walk m) (list 'quote (meta m))))))

(defmacro condm* [[lhs rhs & ms]]
  `(if-let [{:syms ~(vec (concat (vars lhs)
                                (bound-vars lhs)
                                (map var-sym (regex-vars lhs))))}
            (mimir.well/match ~'*match* ~lhs)]
     ~rhs
     ~(when ms
        `(condm* ~ms))))

(defmacro condm [x & ms]
  `(let [~'*match* ~x]
     (condm* ~ms)))

(defn single-arg? [ms]
  (not-any? coll? (take-nth 2 ms)))

(defmacro fm [& ms]
  `(fn ~'this [& ~'args]
     (condm (if ~(single-arg? ms) (first ~'args) ~'args) ~@ms)))

(defmacro defm [name & ms]
  (let [[doc ms] (split-with string? ms)]
    `(do
       (defn ~name [& ~'args]
         (condm (if ~(single-arg? ms) (first ~'args) ~'args) ~@ms))
       (when '~doc
         (alter-meta! (var ~name) merge {:doc (apply str '~doc)}))
       ~name)))

(defn version []
  (-> "project.clj" clojure.java.io/resource
      slurp read-string (nth 2)))

(defn -main [& args]
  (println)
  (println "Welcome to Mímir |" (version) "| Copyright © 2012 Håkan Råberg")
  (println)
  (require 'clojure.main)
  (clojure.main/repl :init #(in-ns 'mimir.well)))

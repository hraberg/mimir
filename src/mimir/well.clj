(ns mimir.well
  (:use [clojure.set :only (intersection map-invert rename-keys difference union join)]
        [clojure.tools.logging :only (debug info warn error spy)]
        [clojure.walk :only (postwalk postwalk-replace)]
        [mimir.match :only (filter-walk maybe-singleton-coll match all-vars *match-var?*)])
  (:require [clojure.core.reducers :as r])
  (:refer-clojure :exclude [assert])
  (:gen-class))

(defn create-net []
  {:productions #{}
   :working-memory #{}
   :predicates {}
   :predicate-invokers {}
   :expression-cache {}
   :alpha-network {}
   :beta-join-nodes {}})

(def ^:dynamic *net* (atom (create-net)))

(defn dbg [x] (println x) x)

(doseq [k (keys @*net*)]
  (eval `(defn ~(symbol (name k)) [] (~k @*net*))))

(defn triplet? [x]
  (and (sequential? x) (= 3 (count x)) (symbol? (second x))))

(defn is-var? [x]
  (when-let [^String s (and (symbol? x) (name x))]
    (or (.startsWith s "?")
        (re-matches #"[A-Z]+" s))))

(defn var-sym [x]
  (symbol (str "?" x)))

(alter-var-root #'*match-var?* (constantly (every-pred *match-var?* (complement is-var?))))

(defn is-matcher? [x xs]
  (and (is-var? x) (not (symbol? (first xs)))))

(defn matcher? [c]
  (and (sequential? c)
       (= 'mimir.match/match* (first c))))

(defn parser
  ([x] (parser x identity identity))
  ([x atom-fn triplet-fn] (parser x atom-fn triplet-fn true))
  ([[x & xs] atom-fn triplet-fn match]
     (when x
       (cond (and match ((some-fn map? set? vector?) x)) (cons (atom-fn (list 'mimir.match/match (gensym "?") x))
                                                               (parser xs atom-fn triplet-fn match))
             ((some-fn sequential? map? set? string?) x) (cons (atom-fn x)
                                                               (parser xs atom-fn triplet-fn match))
             (and match (is-matcher? x xs)) (cons (atom-fn (list 'mimir.match/match x (first xs)))
                                                  (parser (rest xs) atom-fn triplet-fn match))
             (triplet? (cons x (take 2 xs))) (cons (triplet-fn (cons x (take 2 xs)))
                                                   (parser (drop 2 xs) atom-fn triplet-fn match))
             :else (cons x (parser xs atom-fn triplet-fn match))))))

(defn quote-non-vars [rhs]
  (postwalk #(if (and (symbol? %)
                      (not (is-var? %))) (list 'quote %) %) rhs))

(defn vars [x] (filter-walk is-var? x))

(defn quote-fact [t]
  (list 'quote t))

(defn expand-rhs [t]
  (cons 'mimir.well/assert t))

(def relations (reduce (fn [m rel] (assoc m rel rel))
                       '{<- mimir.well/bind = mimir.match/match* != not=} '[< > <= => not=]))

(defn macroexpand-conditions [lhs]
  (loop [[c & cs] (map macroexpand lhs)
         acc []]
    (if-not c
      acc
      (recur cs
             (if (every? seq? c)
               (into acc c)
               (conj acc c))))))

(defn expand-lhs [t]
  (if-let [rel (relations (second t))]
    (let [[var _ & [rest]] t]
      (if-let [rest (and (seq? rest)
                         (macroexpand-conditions [rest]))]
          (concat (butlast rest) [(list rel var (last rest))])
        (list rel var rest)))
    t))

(defn ellipsis
  ([x] (ellipsis 5 x))
  ([n x]
     (let [[start more] (split-at n (take (inc n) x))]
       (str (seq start)
            (when more
              (str "... [total: " (count x) "]"))))))

(defn binding? [c]
  (and (sequential? c)
       (= 'mimir.well/bind (first c))))

(defn binding-var [c]
  (when (binding? c) (second c)))

(defn binding-vars-for-rule [cs]
  (set (map binding-var (filter binding? cs))))

(defn purge-match-vars [xs]
  (let [match-vars (remove is-var? (keys xs))]
    (apply dissoc xs (concat (map var-sym match-vars) match-vars))))

(defmacro rule [name & body]
  (let [body (if ('#{=>} (first body)) (cons (list (gensym "?") '<- true) body) body)
        [body salience] (if (#{:salience} (first body)) [(drop 2 body) (second body)] [body 0])
        [lhs _ rhs] (partition-by '#{=>} body)
        [doc lhs] (split-with string? lhs)
        expanded-lhs (->> (macroexpand-conditions (parser lhs expand-lhs expand-lhs))
                          (map #(with-meta % {:ns *ns*})))
        rhs (parser rhs identity expand-rhs false)
        binding-vars (binding-vars-for-rule expanded-lhs)]
    `(let [f# (defn ~name
                ([] (~name {}))
                ([{:syms ~(vec (vars lhs)) :as ~'args}] (~name (working-memory) ~'args))
                ([~'wm ~'args]
                   (debug "rule" '~name '~*ns*)
                   (for [vars# (check-rule '~(vec expanded-lhs) ~'wm ~'args)
                         :let [{:syms ~(vec (concat (all-vars lhs) (vars lhs)))} vars#
                               ~'*matches* (map val (sort-by key (dissoc (purge-match-vars vars#) '~@binding-vars)))]]
                     (do
                       (debug "rhs" vars#)
                       ~@rhs))))]
       (debug "defining rule" '~name)
       (when-not (= '~lhs '~expanded-lhs)
         (debug "expanded" '~lhs)
         (debug "    into" '~expanded-lhs))
       (alter-meta! f# merge {:lhs '~lhs :rhs '~rhs :doc ~(apply str doc) :salience ~salience})
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
  (let [vars-and-match-vars #(set (concat (remove '#{_} (all-vars %)) (vars %)))]
    (intersection (vars-and-match-vars x) (vars-and-match-vars y))))

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

(defn tree-eval-walk [locals]
  (fn [form]
    (condp some [form]
      seq? (with-cache expression-cache form
             (eval form))
      locals (locals form)
      form)))

(defmacro tree-eval [tree]
  (let [locals (keys (select-keys &env (filter-walk symbol? tree)))
        locals (into {} (map #(vector (list 'quote %) %) locals))]
    `(let [real-locals# ~locals]
       (postwalk (tree-eval-walk real-locals#) '~tree))))

(defn uses-*matches*? [c]
  (boolean (some '#{*matches*} (flatten c))))

(defn predicate-for [c]
  (with-cache predicate c
    (let [args (ordered-vars c)
          src `(fn [~@args & [~'*matches*]] ~c)
          meta (meta c)]
      (debug " compiling" c)
      (binding [*ns* (or (:ns meta) *ns*)]
        (with-meta (eval src) (merge meta {:src c :args args :uses-*matches* (uses-*matches*? c)}))))))

(defn alias-match-vars [m]
  (merge m
         (zipmap (map (comp var-sym name) (keys m)) (vals m))))

(defn match-using-predicate [c wme]
  (let [predicate (predicate-for c)]
    (try
      (when-let [result (predicate wme)]
        (debug " evaluated to true" wme)
        (merge
         {'?1 wme}
         (when (matcher? c) (alias-match-vars result))))
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
         symbol? (partial ns-resolve (or (-> c meta :ns) *ns*)))
        (every-pred
         (complement symbol?) ifn?)))))

(defn bind [to expr] expr)
(defn constraint [expr] expr)

(defn constraint? [c]
  (and (sequential? c)
       (= 'mimir.well/constraint (first c))))

(defn multi-var-predicate? [c]
  (and (predicate? c) (or (> (count (vars c)) 1) (constraint? c))))

(defn multi-var-predicate-placeholder [c]
  (let [pred (predicate-for c)]
    (debug " more than one argument, needs beta network")
    (with-meta (zipmap (-> pred meta :args) (repeat pred))
      (assoc (meta pred) :pred pred))))

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

(defn update [fact f & args]
  (let [wm (or (first (filter #(match % fact) (working-memory)))
               fact)]
    (retract* wm)
    (mimir.well/fact (condp some [f]
                       fn? (apply f wm args)
                       vector? (let [[a & _] args
                                     args (if (fn? a) args [(constantly a)])]
                                 (apply update-in wm f args))
                       f))))

(defmacro facts [& wms]
  (when wms
    `(doall
      (for [wm# ~(vec (parser wms identity quote-fact false))]
        (fact wm#)))))

(defn fold-into [ctor coll]
  (r/fold (r/monoid into ctor) conj coll))

(defn matching-wmes
  ([c] (matching-wmes c (working-memory) false))
  ([c wm needs-beta?]
     (debug "condition" c)
     (if (or ((some-fn multi-var-predicate? binding?) c)
             needs-beta?)
       #{(multi-var-predicate-placeholder c)}
       (->> wm
            (map #(match-wme c %))
            (remove nil?)
            set))))

(defn alpha-network-lookup [c wm needs-beta?]
  (with-cache alpha-network c
    (matching-wmes c wm needs-beta?)))

(defn alpha-memory
  ([c] (alpha-memory c (working-memory) false))
  ([c wm needs-beta?]
     (let [var-to-index (var-to-index c)
           vars-by-index (map-invert var-to-index)]
       (->> (alpha-network-lookup (with-meta (postwalk-replace var-to-index c) (meta c)) wm needs-beta?)
            (map #(rename-keys (with-meta % (merge (meta %) (postwalk-replace vars-by-index (meta %)))) vars-by-index))))))

(defn cross [left right]
  (debug " nothing to join on, treating as or")
  (set
   (for [x left y right]
     (merge x y))))

(defn multi-var-predicate-node? [am]
  (and (seq? am) (= 1 (count am))
       (fn? (-> am first meta :pred))))

(defn permutations* [n coll]
  (if (zero? n)
    [[]]
    (->> (permutations* (dec n) coll)
         (r/mapcat #(r/map (fn [x] (cons x %)) coll)))))

(defn permutations
  ([coll] (permutations (count coll) coll))
  ([n coll]
     (fold-into vector (permutations* n coll))))

(defn predicate-invoker [args join-on binding-vars uses-*matches*]
  (with-cache predicate-invokers [args join-on binding-vars uses-*matches*]
    (eval `(fn [pred# {:syms [~@(filter join-on args)] :as matches#}]
             (let [matches# (when ~uses-*matches*
                              (vals (dissoc (purge-match-vars matches#) '~@binding-vars)))]
               (fn [[~@(remove join-on args)]]
                 (pred# ~@args matches#)))))))

(defn deal-with-multi-var-predicates [c1-am c2-am join-on c2 binding-vars]
  (let [pred (-> c2-am first meta :pred)
        args (-> c2-am first meta :args)
        bind-var (binding-var c2)
        matcher ((some-fn matcher? constraint? c2))
        uses-*matches* (-> pred meta :uses-*matches*)
        join-on (if bind-var (conj join-on bind-var) join-on)
        needed-args (vec (remove join-on args))
        permutated-wm (permutations (count needed-args) (working-memory))
        invoker (predicate-invoker args join-on binding-vars uses-*matches*)
        join-fn (fn [m]
                  (let [invoker (invoker pred m)]
                    (->> permutated-wm
                         (r/map (fn [wm]
                                  (try
                                    (when-let [r (invoker wm)]
                                      (merge m
                                             (zipmap needed-args wm)
                                             (when matcher
                                               (alias-match-vars r))
                                             (when bind-var
                                               {bind-var r})))
                                    (catch RuntimeException e
                                      (debug " threw non fatal" e)))))
                         (r/remove nil?))))]
    (debug " multi-var-predicate")
    (debug " args" args)
    (debug " known args" join-on "- need to find" needed-args)
    (debug " permutations of wm" (ellipsis permutated-wm))
    (->> c1-am
         (r/mapcat join-fn)
         (fold-into vector))))

(defn beta-join-node [c1 c2 c1-am binding-vars wm]
  (let [c2-am (alpha-memory c2 wm (some binding-vars (vars c2)))]
    (with-cache beta-join-nodes [c1-am c2-am]
      (let [join-on (join-on (-> c1-am first keys) c2)]
        (debug "join" join-on)
        (debug "  left" (ellipsis c1-am))
        (debug " right" (ellipsis c2-am))
        (let [result (cond
                      (multi-var-predicate-node? c2-am) (deal-with-multi-var-predicates
                                                          c1-am c2-am
                                                          join-on c2 binding-vars)
                      (empty? join-on) (cross c1-am c2-am)
                      :else (join c1-am c2-am))]
          (debug "result" (ellipsis result))
          result)))))

(defn dummy-beta-join-node [c wm args binding-vars]
  (beta-join-node [] c #{args} binding-vars wm))

(defn order-conditions [cs]
  (mapcat #(concat (sort-by (comp count vars) (remove constraint? %))
                   (filter constraint? %))
          (partition-by binding? cs)))

(defn check-rule [cs wm args]
  (debug "conditions" cs)
  (let [binding-vars (binding-vars-for-rule cs)]
    (loop [[c1 & cs] (order-conditions cs)
           matches (dummy-beta-join-node c1 wm args binding-vars)]
      (if-not cs
        matches
        (let [c2 (first cs)]
          (recur cs (beta-join-node c1 c2 matches binding-vars wm)))))))

(defn salience [p]
  (or (-> p meta :salience) 0))

(defn run-once
  ([] (run-once (working-memory) (productions)))
  ([wm productions]
     (->> productions (sort-by salience) vec
          (r/mapcat #(% wm {}))
          (fold-into vector))))

(defn run*
  ([] (repeatedly run-once)))

(defn run
  ([] (run *net*))
  ([net]
     (binding [*net* net]
       (loop [wm (working-memory)
              productions (:productions @net)
              acc #{}]
         (let [acc (union (set (run-once wm productions)) acc)]
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

(defn different* [f xs]
  (apply distinct? (map f (maybe-singleton-coll xs))))

(defmacro different
  ([f] `(different ~f ~'*matches*))
  ([f xs]
     (if ((some-fn set? vector?) f)
       (map #(do `(constraint (different ~% ~xs))) f)
       `(constraint (different* ~f ~xs)))))

(defmacro all-different
  ([] `(different identity))
  ([& xs]
     `(different identity ~(vec xs))))

(defn same*
  ([test pred xs]
     (test (for [x xs y (remove #{x} xs)]
             (pred x y)))))

(defmacro not-same
  ([pred] `(not-same ~pred ~'*matches*))
  ([pred xs]
     (if ((some-fn set? vector?) pred)
       (map #(do `(constraint (not-same ~% ~xs))) pred)
       `(constraint (same* (partial not-any? true?) ~pred (maybe-singleton-coll ~xs))))))

(defn same [pred & xs]
  (if ((some-fn set? vector?) pred)
    (map #(list 'same % xs) pred)
    `(same* (partial every? true?) ~pred (maybe-singleton-coll ~xs))))

(defmacro gen-vars
  ([n] `(gen-vars ~n ~(gensym)))
  ([n prefix]
     `(vec (map #(var-sym (str '~prefix "-" %))
                (range 1 (inc ~n))))))

(defmacro unique [xs]
  (concat
   (for [[x y] (partition 2 1 xs)]
     `(pos? (compare ~x ~y)))
   (list (list 'identity xs))))

(defmacro take-unique [n]
  `(unique ~(gen-vars (eval n))))

(defmacro take-distinct [n]
  `(identity ~(gen-vars (eval n))))

(defn not-in [set]
  (complement set))

(defn is-not [x]
  (partial not= x))

(defmacro constrained-match [m x]
  `(some #(match % ~m) ~x))

(defmacro constrain
  ([m] `(constraint (constrained-match ~m ~'*matches*)))
  ([x m]`(constraint (constrained-match ~m ~x))))

(defn version []
  (-> "project.clj" clojure.java.io/resource
      slurp read-string (nth 2)))

(defn -main [& args]
  (println)
  (println "Welcome to Mímir |" (version) "| Copyright © 2012 Håkan Råberg")
  (println)
  (require 'clojure.main)
  (clojure.main/repl :init #(in-ns 'mimir.well)))

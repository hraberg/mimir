(ns mimir.parse
  (:require [clojure.core.reducers :as r]
            [flatland.ordered.map :as om]
            [flatland.ordered.set :as os])
  (:import [java.util.regex Pattern]
           [java.util Map Set List]
           [clojure.lang Keyword ArityException]
           [flatland.ordered.set OrderedSet]))

;; Mímir Parse

;; Experimental parser, this isn't built on some nice theoretical basis.
;; Inspired by the awesome Instaparse: https://github.com/Engelberg/instaparse

;; Started out as an experiment in Ced: https://github.com/hraberg/ced/
;; This parser like the other parts of Mímir was written as a learning exercise.

;; See mimir.test.parse for examples (in various broken states).

(set! *warn-on-reflection* true)

(declare node maybe-singleton)

(def ^:dynamic *allow-split-tokens* true) ;; Overrides post-delimiter.
(def ^:dynamic *memoize* true)
(def ^:dynamic *capture-string-literals* false)
(def ^:dynamic *pre-delimiter* #"\s*")
(def ^:dynamic *post-delimiter* #"(:?\s+|$)")
(def ^:dynamic *offset* 0)
(def ^:dynamic *rule* nil)
(def ^:dynamic *default-result* [])
(def ^:dynamic *token-fn* conj)
(def ^:dynamic *suppress-tags* false)
(def ^:dynamic *node-fn* #'node)
(def ^:dynamic *default-action* #'maybe-singleton)
(def ^:dynamic *grammar-actions* true)
(def ^:dynamic *alternatives-rank* (comp count flatten :result))
(def ^:dynamic *grammar* {})
(def ^:dynamic *failure-grammar* {:no-match [#"\S*" #(throw (IllegalStateException. (str "Don't know how to parse: " %)))]})
(def ^:dynamic *start-rule* first)
(def ^:dynamic *extract-result* (comp first :result))
(def ^:dynamic *rules-seen-at-point* #{})

(defn maybe-singleton
  ([])
  ([x] x)
  ([x & args] (vec (cons x args))))

(defn suppressed-rule? [r]
  (when-let [[ _ r] (re-find #"^<(.+)>$" (name r))]
    (keyword r)))

(defn node? [x]
  (and (vector? x) (keyword? (first x))))

(defn node [& args]
  (let [args (apply maybe-singleton args)]
    (if (or *suppress-tags* (suppressed-rule? *rule*))
      args
      (if (and (sequential? args) (not (node? args)))
        (vec (cons *rule* args))
        [*rule* args]))))

(defn suppressed-defintion? [r]
  (let [suppressed-defintion (keyword (str "<" (name r) ">"))]
    (if (*grammar* suppressed-defintion)
      suppressed-defintion
      r)))

(defrecord StringParser [string offset token result])

(defn string-parser
  ([s] (if (instance? StringParser s) s (string-parser s *default-result*)))
  ([s result] (StringParser. s 0 nil result)))

(defn at-end? [{:keys [string offset] :as in}]
  (= offset (count string)))

(defn try-parse [{:keys [string offset result] :as in} ^Pattern re]
  (when in
    (let [m (re-matcher re (subs string offset))]
      (when (.lookingAt m)
        (assoc in
          :offset (+ offset (.end m 0))
          :token (.group m 0))))))

(defn try-parse-skip-delimiter [in m]
  (when-let [{:keys [token] :as in} (if-let [result (try-parse in m)]
                                      result
                                      (-> in
                                          (try-parse *pre-delimiter*)
                                          (try-parse m)))]
    (when-let [in (if *allow-split-tokens* in (try-parse in *post-delimiter*))]
      (assoc in :token token))))

(defn next-token [in m capture?]
  (when-let [{:keys [token offset] :as in} (try-parse-skip-delimiter in m)]
    (assoc (if capture?
             (binding [*offset* offset]
               (->  in
                    (update-in [:result] *token-fn* token)))
             in) :token nil)))

(defn name-and-predicate [n]
  (let [[_ predicate n] (re-find #"^([!&]?)(.+)" (name n))]
    [(keyword n) (when (seq predicate) (symbol predicate))]))

(defn name-and-quantifier [n]
  (let [[_ n quantifier] (re-find #"(.+?)([+*?]?)$" (name n))]
    [(keyword n) (when (seq quantifier) (symbol quantifier))]))

;; Not sure this name is right
(defprotocol IParser
  (parse [this] [this in]))

(defn fold-into [ctor coll]
  (r/fold (r/monoid into ctor) conj coll))

;; This could potentially be a tree, but requires to restructure and use reducers all over the place.
(defn valid-choices [in ms]
  (fold-into vector (r/remove nil? (r/map #(parse % in) (vec ms)))))

(extend-protocol IParser
  Pattern
  (parse [this in]
    (next-token in this true))

  Character
  (parse [this in]
    (parse (str this) in))

  String
  (parse
    ([this] (parse (string-parser this)))
    ([this in]
       (next-token in (re-pattern (Pattern/quote this)) *capture-string-literals*)))

  Keyword
  (parse [this in]
    (when-not (*rules-seen-at-point* [this in])  ;; Only guards against StackOverflow, doesn't actually handle left recursion.
      (binding [*rules-seen-at-point* (conj *rules-seen-at-point* [this in])]
        (let [[this quantifier] (name-and-quantifier this)
              [this predicate] (name-and-predicate this)
              suppressed (suppressed-rule? this)
              this (suppressed-defintion? this)]
          (if-let [[rule action] (some *grammar* [this suppressed])]
            (letfn [(parse-one [in]
                      (let [current-result (:result in)]
                        (when-let [result (parse rule (assoc in :result *default-result*))]
                          (binding [*rule* this]
                            (update-in result [:result]
                                       #(*token-fn* current-result
                                                    (*node-fn* (try
                                                                 (apply (or (when *grammar-actions* action)
                                                                            *default-action*) %)
                                                                 (catch ArityException _
                                                                   (apply *default-action* %))))))))))
                    (parse-many [in quantifier]
                      (case quantifier
                        ? (or (parse-one in) in)
                        * (loop [in in]
                            (if-let [in (parse-one in)]
                              (recur in)
                              in))
                        + (when-let [in (parse-one in)]
                            (parse-many in '*))
                        (parse-one in)))]
              (let [result (parse-many in quantifier)]
                (case predicate
                  ! (when-not result in)
                  & (when result in)
                  result)))
            (throw (IllegalStateException. (str "Unknown rule: " this))))))))

  Set
  (parse [this in]
    (when-let [alternatives (seq (distinct (valid-choices in this)))]
      (apply max-key :offset (sort-by *alternatives-rank* alternatives))))

  OrderedSet
  (parse [this in]
    (first (valid-choices in this)))

  Map
  (parse [this in]
    (binding [*grammar* this]
      (parse (*start-rule* (os/into-ordered-set (keys this))) (string-parser in))))

  List
  (parse [this in]
    (loop [in in
           [m & m-rst] this]
      (if (and in m (not (at-end? in)))
        (recur (parse m in) m-rst)
        (when-not m in))))

  StringParser
  (parse
    ([this] (parse *grammar* this))
    ([this parser]
       (parse parser this))))

(def choice os/ordered-set)

(defn fun [s]
  (resolve (symbol s)))

(defn op
  ([op x] ((fun op) x))
  ([x op y] ((fun op) x y)))

;; This feels a bit clunky
(defmacro dynamic-reader []
  (let [locals (vec (keys &env))]
    `#(eval `(let [~'~locals ~~locals]
               ~(read-string %)))))

(def ^:dynamic *dynamic-reader*)

(defn action? [x]
  ((some-fn fn? var?) x))

(defn rule? [r]
  (and (vector? r) (= 2 (count r)) (action? (last r))))

(defn grammar [& rules]
  (let [rules (mapcat (fn [[rs [f]]] (if f (conj (vec (butlast rs)) [(last rs) f]) rs))
                      (partition-all 2 (partition-by action? rules)))]
    (into (om/ordered-map) (map (fn [[name rule]] [name (if (rule? rule)
                                                          rule
                                                          [rule])])
                                (partition 2 rules)))))

(defn parser-options [options]
  (into {} (map (fn [[k v]]
                  [(if (keyword? k)
                     (or (resolve (symbol (str "*" (name k) "*")))
                         (throw (IllegalArgumentException. (str "Unknown option: " k))))
                     k) v]) options)))

;; Starts getting clunky, holding off to macrofiy it as this is not the core issue.
(defn create-parser
  ([& rules]
     (let [[[default-options] rules] (split-with map? rules)
           default-options (parser-options default-options)
           grammar (apply grammar rules)]
       (fn parser
         ([in & options]
            (with-bindings (merge default-options (parser-options (apply hash-map options)))
              (let [real-parse parse]
                (try
                  (when *memoize* ;; Just rebinding doesn't work for some reason
                    (alter-var-root #'parse memoize))
                  (when-let [in (parse grammar in)]
                    (if (at-end? in)
                      (*extract-result* in)
                      (parse *failure-grammar* in)))
                  (finally
                   (when *memoize*
                     (alter-var-root #'parse (constantly real-parse))))))))))))

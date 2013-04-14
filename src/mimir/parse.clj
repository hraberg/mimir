(ns mimir.parse
  (:require [clojure.core.reducers :as r]
            [clojure.walk :as w]
            [flatland.ordered.map :as om]
            [flatland.ordered.set :as os])
  (:import [java.util.regex Pattern]
           [java.util Map Set List]
           [clojure.lang Keyword ArityException Fn]
           [flatland.ordered.set OrderedSet]))

;; Mímir Parse

;; Experimental parser, this isn't built on some nice theoretical basis.
;; Inspired by the awesome Instaparse: https://github.com/Engelberg/instaparse

;; Started out as an experiment in Ced: https://github.com/hraberg/ced/
;; This parser like the other parts of Mímir was written as a learning exercise.

;; See mimir.test.parse for examples (in various broken states).

(set! *warn-on-reflection* true)

(declare node maybe-singleton depth column)

(def ^:private ^:dynamic *rule* nil)
(def ^:private ^:dynamic *current-state* nil)
(def ^:private ^:dynamic *rules-seen-at-point* #{})

(def ^:dynamic *allow-split-tokens* true) ;; Overrides post-delimiter.
(def ^:dynamic *memoize* true)
(def ^:dynamic *capture-literals* false)
(def ^:dynamic *pre-delimiter* #"\s*")
(def ^:dynamic *post-delimiter* #"(:?\s+|$)")
(def ^:dynamic *first-line* 1)
(def ^:dynamic *default-result* [])
(def ^:dynamic *token-fn* conj)
(def ^:dynamic *suppress-tags* false)
(def ^:dynamic *node-fn* #'node)
(def ^:dynamic *default-action* #'maybe-singleton)
(def ^:dynamic *actions* {})
(def ^:dynamic *grammar-actions* true)
(def ^:dynamic *alternatives-rank* #'depth)
(def ^:dynamic *grammar* {})
(def ^:dynamic *failure-grammar* {:no-match [#"\S+" #(throw (IllegalStateException.
                                                             (format "Don't know how to parse: '%s' at %d:%d"
                                                                     % (:line *current-state*) (column *current-state*))))]})
(def ^:dynamic *start-rule* first)
(def ^:dynamic *extract-result* (comp first :result))
(def ^:dynamic *read-string* read-string)


;; Not sure this name is right
(defn maybe-singleton
  ([])
  ([x] x)
  ([x & args] (when-let [v (seq (remove nil? (cons x args)))]
                (vec v))))

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

(defn depth [x]
  (if (node? x) (inc (apply max (map depth x))) 0))

(defn suppressed-defintion? [r]
  (let [suppressed-defintion (keyword (str "<" (name r) ">"))]
    (if (*grammar* suppressed-defintion)
      suppressed-defintion
      r)))

(defprotocol IParser
  (parse [this] [this in]))

(defrecord StringParser [string offset line token result]
  IParser
  (parse [this] (parse *grammar* this))
  (parse [this parser]
    (parse parser this)))

(defrecord ZeroOrMore [m]
  IParser
  (parse [this in]
    (loop [in in]
      (if-let [in (parse m in)]
        (recur in)
        in))))

(defrecord OneOrMore [m]
  IParser
  (parse [this in]
    (when-let [in (parse m in)]
      (parse (ZeroOrMore. m) in))))

(defrecord Optional [m]
  IParser
  (parse [this in]
    (or (parse m in) in)))

(defrecord Not [m]
  IParser
  (parse [this in]
    (and (not (parse m in)) in)))

(defrecord And [m]
  IParser
  (parse [this in]
    (and (parse m in) in)))

;; Not sure what to call these guys.
(def take+ ->OneOrMore)
(def take* ->ZeroOrMore)
(def take? ->Optional)
(def ! ->Not)
(def & ->And)

(defn string-parser
  ([s] (if (instance? StringParser s) s (string-parser s *default-result*)))
  ([s result] (StringParser. s 0 *first-line* nil result)))

(defn at-end? [{:keys [string offset] :as in}]
  (= offset (count string)))

(defn column [{:keys [string offset] :as in}]
  (let [eol (.lastIndexOf ^String string (int \newline) (int (dec offset)))]
    (if (= -1 eol)
      offset
      (dec (- offset eol)))))

(defn lines [s]
  (count (re-seq #"\n" s)))

(defn try-parse [{:keys [string offset result line] :as in} ^Pattern re]
  (when in
    (let [m (re-matcher re (subs string offset))]
      (when (.lookingAt m)
        (assoc in
          :offset (+ offset (.end m 0))
          :line (+ line (lines (.group m 0)))
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
             (binding [*current-state* in]
               (->  in
                    (update-in [:result] *token-fn* token)))
             in) :token nil)))

(defn name-and-predicate [n]
  (let [[_ predicate n] (re-find #"^([!&]?)(.+)" (name n))]
    [(keyword n) (when (seq predicate) (symbol predicate))]))

(defn name-and-quantifier [n]
  (let [[_ n quantifier] (re-find #"(.+?)([+*?]?)$" (name n))]
    [(keyword n) (when (seq quantifier) (symbol quantifier))]))

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
       (next-token in (re-pattern (Pattern/quote this)) *capture-literals*)))

  ;; Aim to implement left recursion (and rewrite memoize) using http://www.dcomp.ufs.br/~sergio/docs/leftpeglist.pdf
  ;; IronMeta contains a C# implementation of the algorithm: http://ironmeta.sourceforge.net/
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
                          (binding [*rule* this
                                    *current-state* in]
                            (update-in result [:result]
                                       #(*token-fn* current-result
                                                    (*node-fn* (try
                                                                 (apply (or (when *grammar-actions*
                                                                              (or (when (contains? *actions* this)
                                                                                    (let [action (this *actions*)]
                                                                                      (if (fn? action)
                                                                                        action
                                                                                        (constantly action))))
                                                                                  (this *actions*)
                                                                                  action))
                                                                            *default-action*) %)
                                                                 (catch ArityException _
                                                                   (apply *default-action* %))))))))))]
              (let [parser (case quantifier
                             ? (Optional. parse-one)
                             * (ZeroOrMore. parse-one)
                             + (OneOrMore. parse-one)
                             parse-one)]
                (parse (case predicate
                         ! (Not. parser)
                         & (And. parser)
                         parser) in)))
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
      (if (and in m)
        (recur (parse m in) m-rst)
        in)))

  Fn
  (parse [this in]
    (this in)))

(def choice os/ordered-set)

(defn fun [s]
  (resolve (symbol (str s))))

(defn op
  ([op x] ((fun op) x))
  ([x op y] ((fun op) x y)))

;; This feels a bit clunky
(defmacro dynamic-reader []
  (let [locals (vec (keys &env))]
    `#(eval `(let [~'~locals ~~locals]
               ~(read-string %)))))

(defn action? [x]
  ((some-fn fn? var?) x))

(defn rule? [r]
  (and (vector? r) (= 2 (count r)) (action? (last r))))

(defn grammar [& rules]
  (let [rules (mapcat (fn [[rs [f]]] (if f (conj (vec (butlast rs)) [(last rs) f]) rs))
                      (partition-all 2 (partition-by action? rules)))]
    (into (om/ordered-map) (map (fn [[name rule]]
                                  [name (if (rule? rule)
                                          rule
                                          [rule])])
                                (partition 2 rules)))))

(defn parser-option [option]
  (letfn [(unknown-option [option] (throw (IllegalArgumentException. (str "Unknown option: " option))))]
    (if (keyword? option)
      (or (when-let [option (resolve (symbol (str "*" (name option) "*")))]
            (when-not (:private (meta option))
              option))
          (unknown-option option))
      option)))

(defn parser-options [options]
  (into {} (map (fn [[k v]] [(parser-option k) v]) options)))

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
                  (or (when-let [in (parse grammar in)]
                        (when (at-end? in)
                          (*extract-result* in)))
                      (parse *failure-grammar* in))
                  (finally
                   (when *memoize*
                     (alter-var-root #'parse (constantly real-parse))))))))))))

;; Should be folded into the above, but requires some messing about.
(defn create-parser-from-map
  ([m] (create-parser-from-map {} m))
  ([options m]
     (apply create-parser options (apply concat m))))

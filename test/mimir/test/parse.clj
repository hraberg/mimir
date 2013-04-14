(ns mimir.test.parse
  (:require [flatland.ordered.map :as om]
            [clojure.string :as s])
  (:use [mimir.parse]))

;; This is not yet a real test, just experiments and examples in various broken states.

;; Figure 1. PEG formally describing its own ASCII syntax
;; from Parsing Expression Grammars: A Recognition-Based Syntactic Foundation
;; http://bford.info/pub/lang/peg.pdf
(def peg-grammar (slurp "test/mimir/test/peg.txt"))

;; Grammar to transform PEG into a Mímir grammar.
;; Some places could be simplified using regular expressions, but trying to keep it close to the original.
;; Note that some actions rely on ArityException falling back to *default-action* instead of providing an implementation.
(def peg-options {:suppress-tags        true
                  :capture-literals     true
                  :pre-delimiter        #""
                  :post-delimiter       #""

                  :actions {:Grammar    (fn [& xs] (apply om/ordered-map (mapcat eval xs)))
                            :Expression (fn ([x] x) ([x & xs] (apply list `choice (remove nil? (cons x xs)))))
                            :Prefix     (fn [prefix x] (list prefix x))
                            :Suffix     (fn [x suffix] (list suffix x))
                            :Primary    (fn [open x close] x)

                            :Identifier (comp keyword str)
                            :Literal    (fn [& xs]
                                          (reduce (fn [s [m r]]
                                                    (s/replace s m r)) (s/replace (apply str xs) #"(^'|'$)" "")
                                                    [["\\\\" "\\"] ["\\n" "\n"] ["\\r" "\r"] ["\\t" "\t"]]))
                            :Class      (fn [& xs] (re-pattern (apply str xs)))
                            :Range      (fn ([start dash end] (str start dash end)))
                            :Char       str

                            :LEFTARROW  nil
                            :SLASH      nil
                            :AND        `&
                            :NOT        `!
                            :QUESTION   `take?
                            :STAR       `take*
                            :PLUS       `take+
                            :OPEN       nil
                            :CLOSE      nil
                            :DOT        #"."

                            :Spacing    nil}})

;; Bootstrap grammar, same as peg.txt in Mimir's format.
(def peg (create-parser
          peg-options

          ;; # Hierarchical syntax
          :Grammar    [:Spacing :Definition+ :EndOfFile]
          :Definition [:Identifier :LEFTARROW :Expression]
          :Expression [:Sequence (take* [:SLASH :Sequence])]
          :Sequence   :Prefix*
          :Prefix     [(take? (choice :AND :NOT)) :Suffix]
          :Suffix     [:Primary (take? (choice :QUESTION :STAR :PLUS))]
          :Primary    (choice [:Identifier (! :LEFTARROW)]
                              [:OPEN :Expression :CLOSE]
                              :Literal :Class :DOT)
          ;; # Lexical syntax
          :Identifier [:IdentStart :IdentCont* :Spacing]
          :IdentStart #"[a-zA-Z_]"
          :IdentCont  (choice :IdentStart #"[0-9]")
          :Literal    (choice ["'" (take* [(! "'") :Char]) "'" :Spacing]
                              ["\"" (take* [(! "\"") :Char]) "\"" :Spacing])
          :Class      ["[" (take* [(! "]")  :Range]) "]" :Spacing]
          :Range      (choice [:Char "-" :Char] :Char)
          :Char       (choice [#"\\" #"[nrt'\"\[\]\\]"]
                              [#"\\" #"[0-2][0-7][0-7]"]
                              [#"\\" #"[0-7][0-7]?"]
                              [(! "\\") #"."])

          :LEFTARROW  ["<-" :Spacing]
          :SLASH      ["/" :Spacing]
          :AND        ["&" :Spacing]
          :NOT        ["!" :Spacing]
          :QUESTION   ["?" :Spacing]
          :STAR       ["*"  :Spacing]
          :PLUS       ["+" :Spacing]
          :OPEN       ["(" :Spacing]
          :CLOSE      [")" :Spacing]
          :DOT        ["." :Spacing]

          :Spacing    (take* (choice :Space :Comment))

          :Comment    ["#" (take* [(! :EndOfLine) #"."]) :EndOfLine]
          :Space      (choice " " "\t"  :EndOfLine)
          :EndOfLine  (choice "\r\n" "\n" "\r")
          :EndOfFile  (! #".")))

;; We use the bootstrap to parse the real PEG grammar:
(def mimir-peg (peg peg-grammar))

;; Reparse the grammar using the created parser:
(def peg-peg ((create-parser-from-map peg-options mimir-peg) peg-grammar))

;; mimir-peg and peg-peg are now functionally equal, but contains fns that aren't, string equality holds:
(= (pr-str mimir-peg) (pr-str peg-peg))

;; Not Memoizing is faster here:
(comment
  (time (peg peg-grammar :memoize false)))

;; This grammar is from the "Transforming the tree" section of Instaparser:
;; https://github.com/Engelberg/instaparse#transforming-the-tree

(def expression (create-parser
                 :expr      :add-sub
                 :<add-sub> #{:mul-div :add :sub}
                 :add       [:add-sub "+" :mul-div]
                 :sub       [:add-sub "-" :mul-div]
                 :<mul-div> #{:term :mul :div}
                 :mul       [:mul-div "*" :term]
                 :div       [:mul-div "/" :term]
                 :<term>    #{:number ["(" :add-sub ")"]}
                 :number    #"[0-9]+"))

(expression "1")

;; Stopped working after starting from first rule only, now needs left recurision.
;(expression "1/2")
;(expression "2+5*2")
;; Doesn't work yet
;(expression "1+2+3")
;; Need to handle left recursion, tree from instaparse:
;; [:expr [:add [:add [:number "1"] [:number "2"]] [:number "3"]]]

;;(expression "1-2/(3-4)+5*6")

;; PEG example from http://bford.info/pub/lang/packrat-icfp02-slides.pdf
;; Additive → Multitive '+' Additive
;; | Multitive
;; Multitive → Primary '*' Multitive
;; | Primary
;; Primary → '(' Additive ')'
;; | Decimal
;; Decimal → '0' | ... | '9'

(def peg-expression (create-parser
                     {:suppress-tags true}

                     :additive  (choice [:multitive #"[+-]" :additive]
                                        :multitive) op
                     :multitive (choice [:primary #"[*/]" :multitive]
                                        :primary) op
                     :primary   (choice ["(" :additive ")"]
                                        :decimal)
                     :decimal   #"[0-9]+" read-string))

;; This gets wrong precedence, regardless of using choice / OrderedSet or not. So something else.
;; This also seems right associtive?
;; Should return 33.
(peg-expression "1-2/(3-4)+5*6")

(peg-expression "2+5*2")
(peg-expression "2+5*2" :grammar-actions false :suppress-tags false)

;; A different expression grammar from:
;; http://www.cs.umd.edu/class/fall2002/cmsc430/lec4.pdf

;; Left recursive
;; 1 <goal> ::= <expr>
;; 2 <expr> ::= <expr> + <term>
;; 3 | <expr> - <term>
;; 4 | <term>
;; 5 <term> ::= <term> * <factor>
;; 6 | <term> = <factor>
;; 7 | <factor>
;; 8 <factor> ::= number
;; 9 | id

(def left-recursive (create-parser
                     :goal   :expr
                     :expr   #{[:expr #"\+" :term]
                               [:expr #"-" :term]
                               :term}
                     :term   #{[:term #"\*" :factor]
                               [:term #"/" :factor]
                               :factor}
                     :factor #{#"[0-9]+" #"\w+"}))
;; Doesn't work
;; (left-recursive "x - 2 * y")

;; Right recursive
;; 1 <goal> ::= <expr>
;; 2 <expr> ::= <term> + <expr>
;; 3 | <term> - <expr>
;; 4 | <term>
;; 5 <term> ::= <factor> * <term>
;; 6 | <factor> / <term>
;; 7 | <factor>
;; 8 <factor> ::= number
;; 9 | id

(def right-recursive (create-parser
                      {:suppress-tags true}

                      :goal   :expr
                      :expr   #{[:term #"[+-]" :expr]
                                :term} op
                      :term   #{[:factor #"[*/]" :term]
                                ["(" :expr ")"]
                                :factor} op
                      :factor #{#"[0-9]+" #"\w+"} #'*read-string*))

(let [x 1 y 3]
  (right-recursive "x - 2 * y" :read-string (dynamic-reader)))

;; Extended the grammar to support parenthesis, now this fails the same way as peg-expression:
(right-recursive "1-2/(3-4)+5*6")
;; Gives -27, should be 33.

;; Here's a simpler tree that fails:
;; Gives -4, should be 2.
(right-recursive "1-2+3")
;; Actually, the slides says: "Note: This grammar is right-associative." !

;; But the tree is much nicer, and could hint to the answer:
(right-recursive "1-2/(3-4)+5*6" :grammar-actions false :suppress-tags false)

;; Check that memoization actually works.
(comment
  (let [x 1 y 3]
    (time (right-recursive "x - 2 * y" :read-string (dynamic-reader) :memoize false))))


;; This variant handles left-associative without being left recursive:
;; http://stackoverflow.com/questions/6148705/relation-between-grammar-and-operator-associativity?rq=1

;; Expr  ::= Term  ( ("+"|"-") Term )*;
;; Term  ::= Factor ( ( "*" | "/" ) Factor )* ;
;; Factor ::= INTEGER | "(" Expr ")"

;; But the tree is not very clear:
(defn expr-eval [& args]
  (let [args (apply maybe-singleton args)]
    (if (sequential? args)
      (reduce (fn [x [op y]] ((fun op) x y))
              (first args) (partition-all 2 (rest args)))
      args)))

;; As seen above, named after the site, not the exception.
(def stackoverflow (create-parser
                    {:suppress-tags true}

                    :expr    [:term (take* [#"[+-]" :term])] expr-eval
                    :term    [:factor  (take* [#"[*/]" :factor])] expr-eval
                    :factor  #{:integer ["(" :expr ")"]}
                    :integer #"[0-9]+" read-string))

;; Gives 2 as expected:
(stackoverflow "1-2+3")
(stackoverflow "1-2+3" :grammar-actions false :suppress-tags false)
;; Gives -4 as it should:
(stackoverflow "1-(2+3)")
;; Even this beast of mathematical wonder works:
(stackoverflow "1-2/(3-4)+5*6")

;; Mark Engelberg's examples from the Clojure mailing list.
;; (def addition-associate-right
;;   (insta/parser
;;     "plus = num <'+'> plus | num
;;      num = #'[0-9]'+"))

;; (def addition-associate-left
;;   (insta/parser
;;     "plus = plus <'+'> num | num
;;      num = #'[0-9]'+"))

(def addition-associate-right (create-parser
                               {:suppress-tags true}

                               :plus (choice [:num "+" :plus] :num) +
                               :num  #"[0-9]+" read-string))

(addition-associate-right "1+2+3")

(def addition-associate-left (create-parser
                              {:suppress-tags true}

                              :plus (choice [:plus #"[+]" :num] :num) +
                              :num  #"[0-9]+" read-string))

;; This requires left recursion
;; (addition-associate-left "1+2+3")

;; "As example use of our combinators, consider the following ambiguous grammar from Tomita (1986)."
;; http://cs.uwindsor.ca/~richard/PUBLICATIONS/PADL_08.pdf

;; s ::= np vp | s pp   np ::= noun | det noun | np pp
;; pp ::= prep np       vp ::= verb np
;; det ::= "a" | "the"  noun ::= "i" | "man" | "park" | "bat"
;; verb ::= "saw"       prep ::= "in" | "with"
(def ambiguous (create-parser
                {:capture-literals true}

                :s    #{[:np :vp] [:s :pp]}
                :pp   [:prep :np]
                :det  #{"a" "the"}
                :verb "saw"
                :np   #{:noun [:det :noun] [:np :pp]}
                :vp   [:verb :np]
                :noun #{"i" "man" "park" "bat"}
                :prep #{"in" "with"}))

(ambiguous "i saw a man in the park with a bat")

;; Different ways to specify greedy quanitifers, either in the keyword or via a fn.
(def helloworld (create-parser
                 {:capture-literals true}

                 :helloworld [:hello* (take? :world)]
                 :hello "Hello"
                 :world "World"))

(helloworld "Hello Hello World")

;; This grammar is from Mouse, but uses slightly different PEG syntax:
;; http://www.romanredz.se/freesoft.htm
(def c-grammar (slurp "test/mimir/test/C.peg"))
;; (peg c-grammar)

;; Some comments and example about left recursion from the PEG mailing list:
;;

;; I've settled on a strategy that tries to expand out as much left
;; recursion as possible, from left to right.

;; Some example parse trees:

;; S -> SS / a
;; with "aaaa" gives S(S(S(S(a), S(a)), S(a)), S(a))

;; S -> ST / a
;; T -> S
;; with "aaaa" gives S(S(S(S(a), T(S(a))), T(S(a))), T(S(a)))

;; S -> S S T / S c / a  / b
;; T -> d

;; with "abcd" gives S(S(a), S(S(b), c), T(d))
;; with "abcdabcdd gives S(S(S(a), S(S(b), c), T(d)), S(S(a), S(S(b), c),
;; T(d)), T(d))

;; It goes left-to-right in the sense that it will try to expand the
;; first left recursion it encounters. Once it can't keep going with
;; that, it will try to expand out any other left-recursive invocations
;; of the same variable later in the string, regardless of if they appear
;; in the same production as the left recursive invocation.

;; If one needs a right leaning tree with left recursion then now (with
;; my system) it is no longer possible, i.e. left lean for left recursion
;; is forced.

;; A very high level description of my algorithm is that I group the
;; parser stack into chunks representing continuations. When I detect
;; left recursion, I copy of chunk of the top-most continuation on the
;; stack, store it in the memo table, and then advance the topmost parser
;; frame to the next production. When parsing of a variable is completed,
;; all continuations stored in that variable's memo entry at that
;; position are attempted in the order with which they were added into
;; the memo table. Updating the results of a memo table entry (i.e.
;; successfully parsing something) resets the memo entry's next
;; continuation pointer to the first continuation added to the memo
;; entry. The big trick is that if we are in the continuation of a
;; variable (i.e. the size of the continuation stack > 1) and we detect a
;; left recursive invocation of the same variable then instead of adding
;; it to the memo entry for the variable at the current string position,
;; we add it to the memo entry of the start of the current continuation's
;; string position.

;; Best Regards,

;; Peter Goodman,

;; Fun with left recursion

;; Left recursion in PEGs indeed seems like an interesting can of worms.  For those interested, I'm wondering
;; how a few example grammars behave under your preferred left-recursive parsing technique, and how you
;; think they should behave.

;; First, a trivially evil left-recursive grammar:

;; S <- S

;; For example, does your parser detect and reject this somehow, or does it behave the same as 'S <- f'?  (I hope it
;; doesn't result in an infinite loop at runtime anyway. :) )

;; Now a grammar that's weird, not necessarily evil, in a slightly more subtle way:

;; S <- S / a

;; Does this behave the same as 'S <- a', or do something else?  How should it behave?

;; Cranking up the evilness factor one notch with a mutually left-recursive grammar…

;; S <- T / a
;; T <- S / &a

;; Given the input string "a", does this behave the same as 'S <- a' (succeeding and consuming) or the same as 'S
;; <- &a' (succeeding but consuming no input)?  Do S and T behave the same way or differently?  Should they?

;; Now, another grammar that's not necessarily evil but strange in a slightly different way:

;; S <- Saa / a /

;; Given the input string 'aaaa', for example, does/should this grammar consume just 3 or all 4 a's, or does it
;; do something else?  What should it do?

;; Cheers,
;; Bryan
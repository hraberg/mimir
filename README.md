[<img width="154" src=https://github.com/hraberg/mimir/raw/master/resources/mimer.jpg alt="Mímir - Image Copyright © Peter Madsen" title="Mímir - Image Copyright © Peter Madsen" align="right" />](http://www.petermadsen.info/pages/vh/hv-er-hv/mimir.html)

# Mímir

*The god Odin carries around [Mímir's head](http://clojure.org/lazy#Making Clojure Lazier--Don't hang \(onto\) your head) and it recites secret knowledge and counsel to him.*

### Mímir is an experimental rule engine written in Clojure.

[Marginalia](http://ghettojedi.org/mimir/) | [Gittip](https://www.gittip.com/hraberg/)

Mímir aims to implement a Rete network as a base. I don't vouch for its correctness, soundness or anything, actually. Like Mímir surely would attest, using it would be somewhat headless. Here's how it looks:


```clojure
  ; The first example from chapter 2, "The Basic Rete Algorithm" in Doorenbos:
  (facts B1 on B2
         B1 on B3
         B1 color red
         B2 on table
         B2 left-of B3
         B2 color blue
         B3 left-of B4
         B3 on table
         B3 color red)

  (rule find-stack-of-two-blocks-to-the-left-of-a-red-block
        ?x on ?y
        ?y left-of ?z
        ?z color red
        =>
        ?x is on-top)

  (match? B1 is on-top)
```

[This example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/blocks.clj) uses basic triplets, where each value in a fact is a Clojure atom, and in a rule a condition an atom or a var, prefixed with `?`. This mode is the raw mode the Rete network is operating in, but is somewhat limited in it's applicability. In theory, other representations are possible to compile into this format, but no work has been done on making it so, as I'm doubtful about the practical use case for the triplets.

The test macro `match?` uses `mimir.well/run` under the hood, which keeps running (potentially forever) until the working memory is stable. The values returned by run are the returned values of the right hand side bodies, which may not have been added to the working memory. When using triplets, a bare triplet returned on the right hand side is automatically `assert`ed into the working memory, but this isn't the case when returning normal Clojure data structures.

```clojure
  ; Dudeney's SEND + MORE = MONEY:
  (integers)

  (rule send-more-money
        (base 10    S E N D
                  + M O R E
                = M O N E Y)

        (all-different S E N D M O R Y)

        =>

        (str S E N D '+ M O R E '= M O N E Y))

   (match? "9567+1085=10652")
```

[This example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/arithmetic.clj) uses real Clojure code as its conditions. The left hand side, before the `=>`, contains of one or more conditions, which all must be satisfied for the rule to fire the right hand side, the code after `=>`. The right hand side is normal Clojure code, which will be invoked once for each matching set of variables found by the left hand side (in this case, only once). `(integers)` fills the working memory with 10 single digit facts.

`base` is a macro that expands into many more conditions, and introduces variables for the reminders of the addition to limit the amount of unknown variables that has to be found at any given moment. `all-different` is just `distinct?`, but could also be written as a macro expanded into to several sub conditions.

```clojure
  ; N Queens
  (chessboard *n*)

  (rule n-queens

        (take-unique *n*)
        (different #{file rank})
        (not-same diagonal?)

        =>

        (map file *matches*))

  ; n = 5
  (match? [4 2 5 3 1] [3 5 2 4 1] [5 3 1 4 2] [4 1 3 5 2] [5 2 4 1 3]
          [1 4 2 5 3] [2 5 3 1 4] [1 3 5 2 4] [3 1 4 2 5] [2 4 1 3 5])
```

[This example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/n_queens.clj) demonstrates a group of `*n*` queens that are selected by the `take-unique` macro. This expands into several conditions to ensure that the set of working memory elements picked are unique regardless of variable "position". This is done using `compare` behind the scenes in the expanded conditions.

`different` is a macro expanding into a `distinct?` call for each fn. `not-same` is a binary predicate which ensures `diagonal?` isn't `true` for any combinations of queens. This could be expanded into several conditions, but isn't at the moment; there's a balance between brute force search and the overhead of doing more joins - still to be explored.

Evaluation of `mimir.well/run-once` is lazy, so you can do: `(take 1 (n-queens))` when calling a rule directly. In contrast, all results are realized by `mimir.well/run` each iteration to figure out if another run is needed.

And as [Martin](http://martinsprogrammingblog.blogspot.co.uk/) pointed out, this example is "at least two orders of magnitude" too slow!

```clojure
  ; Rosencrantz' problem from chapter 1, "Rules to the Rescue" in Jess in Action:
  (doseq [name ["Fred" "Joe" "Bob" "Tom"]
          pants-color [:red :blue :plaid :orange]
          position (range 1 (inc 4))]
    (fact {:name name :position position :pants-color pants-color}))

  (rule find-solution
        {:name "Fred"
         :position fred}

        {:name "Joe"
         :position 2}

        {:name "Bob"
         :pants-color :plaid}

        {:name "Tom"
         :position (not-in #{1 4})
         :pants-color (is-not :orange)}

        (constrain {:position (inc ?fred)
                    :pants-color :blue})

        (different #{:position :pants-color})

        =>

        (set *matches*))

  (match? #{{:name "Fred", :position 1, :pants-color :orange}
            {:name "Joe", :position 2, :pants-color :blue}
            {:name "Bob", :position 4, :pants-color :plaid}
            {:name "Tom", :position 3, :pants-color :red}})
```

[This example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/golfers.clj) is demonstrating the pattern matcher (see below) operating on normal Clojure maps. `not-in` and `is-not` are predicates for the values. Keys not specified in the match are ignored. The maps introduces new (anonymous) variables, matching the working memory, while the `constrain` and `different` macros works on the current set of matches, not the working memory itself.

For more, see [`mimir.test`](https://github.com/hraberg/mimir/tree/master/test/mimir/test).

### Pong

[This example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/pong.clj) is an attempt to write something less trivial where the working memory keeps changing. It doesn't fully work yet but has shown a few weaknesses in the assumptions made in Mímir which needs addressing. It uses [`clojure-lanterna`](https://github.com/sjl/clojure-lanterna/) for text UI.

    lein trampoline run -m mimir.test.pong

[<img src=https://github.com/hraberg/mimir/raw/master/resources/pong.png alt="Mímir Pong" title="Mímir Pong" />](https://github.com/hraberg/mimir/blob/master/test/mimir/test/pong.clj)

**Known Issues**

* The computer occasionally gets stuck or can only move in one direction
* Some variations of conditions that seem valid just doesn't work as expected (potentially related to the above).
* The match vars are bound to normal vars using a simple aliasing hack, hence the name mismatch (`dx` vs `?dx`).
* Using :swing doesn't work properly.
* Resizing the window resets the game, and leaves some noise on the screen.


#### Pattern Matching

Mimir contains an even more experimental [pattern matcher](https://github.com/hraberg/mimir/blob/master/src/mimir/match.clj), which can be seen in action on maps in the [Rosencrantz golfers example](https://github.com/hraberg/mimir/blob/master/test/mimir/test/golfers.clj) and in [Pong](https://github.com/hraberg/mimir/blob/master/test/mimir/test/pong.clj) above. This pattern matcher and it's relationship and influence on Mimir proper is still a bit up in the air. It can be used on it's own:

```clojure
(defm member? [x & y]
  [x & _ ]  true
  [_ & xs]  (member? x xs))

(defm filter-m [pred & coll]
  [^x pred & xs] (cons x (filter-m pred xs))
  [_       & xs] (filter-m pred xs)
  empty?         ())

(defm map-m [f & coll]
  [x & xs] (cons (f x) (map-m f xs)))

(defm reduce-m [f val & coll]
  [x & xs] (reduce-m f (f x val) xs)
  empty?   val)

(defn factorial [x]
  (condm x
         0 1
         x (* x (factorial (dec x)))))
```

It currently performs the match on the var arg by an arbitrary convention, and can use meta data tags to introduce new bindings in a match.
A symbol which isn't already bound will also introduce a binding, like in `member?` above, `x` matches the actual `x` argument to the fn, but `xs` creates a new var bound to the rest.

When used inside rules, the bindings currently has to be referenced with a `?` prefix in other conditions, for examples, see [Pong](https://github.com/hraberg/mimir/blob/master/test/mimir/test/pong.clj).

No performance tuning has been made - partly because there are no tests for this beast yet.


#### Goals: mímirKanren

Mímir contains some initial functionality to write goals in ["mímirKanren"](https://github.com/hraberg/mimir/blob/master/src/mimir/mk.clj), based on [miniKanren](http://gradworks.umi.com/3380156.pdf), using Mimir's [matcher](https://github.com/hraberg/mimir/blob/master/src/mimir/match.clj) to unify. This was inspired by seeing David Nolen's [unsession](http://www.youtube.com/watch?v=A7de6pC-tnU) on [core.logic](https://github.com/clojure/core.logic/) at StrangeLoop 2012. There's currently no clear way to use this together with Mimir proper, early days.


```clojure
(run* [q w z]
  (consᵒ q w z)
  (firstᵒ z 1))
           ⇒ '(1 –₀ (1 . –₀))

(run 3 [q]
  (memberᵒ 3 q))
           ⇒ '((3 . –₀) (–₀ 3 . –₁) (–₀ –₁ 3 . –₂))

(run* [q]
  (appendᵒ [1 2] q [1 2 3 4]))
           ⇒ '((3 4))
```

#### Parsing

Mímir now also contains an [experimental parser](https://github.com/hraberg/mimir/blob/master/src/mimir/parse.clj), inspired by the awesome [Instaparse](https://github.com/Engelberg/instaparse) by Mark Engelberg.

See [`mimir.test.parse`](https://github.com/hraberg/mimir/blob/master/test/mimir/test/parse.clj) for examples (but an absolute lack of proper tests). Many things doesn't work properly yet, and the theoretical foundations are shaky to say the least. It doesn't support left-recursion - and a few things are broken. I'm currently backing off to read a few papers, so the references list will hopefully be updated in a few days, once I understand more about what I don't understand.

The idea is to eventually fold this together with Mímir's normal matcher so rules can descend into strings as well, inspired by [OMeta](http://tinlizzie.org/ometa/).

```clojure
(def right-recursive                   ;; Note: right associative.
  (create-parser                       ;; This returns a parser function.
   {:suppress-tags true}               ;; Options, can also be given when invoking, see below.

   :goal   :expr                       ;; A rule which is just an alias.
   :expr   #{[:term #"[+-]" :expr]     ;; Sets are (unordered) choices. Keywords refer to rules.
             :term} op                 ;; op is the action, invoked with the result of the parse.
   :term   #{[:factor #"[*/]" :term]
             :factor} op               ;; op resolves the regexp match to clojure.core/* etc.
   :factor #{#"[0-9]+" #"\w+"} #'*read-string*))

(let [x 1 y 3]
  (right-recursive "x - 2 * y" :read-string (dynamic-reader))) ;; dynamic-reader wraps read-string + local scope.
;=> -5
```
This example is (somewhat changed) from these [lecture notes](http://www.cs.umd.edu/class/fall2002/cmsc430/lec4.pdf) form Univeristy of Maryland.


## References

[Production Matching for Large Learning Systems](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf) Robert B. Doorenbos, 1995

[Jess in Action: Java Rule-based Systems](http://www.manning.com/friedman-hill/) Ernest Friedman-Hill, 2003

[OPS5](https://github.com/briangu/OPS5) "This Common Lisp version of OPS5 is in the public domain.  It is based in part on based on a Franz Lisp implementation done by Charles L. Forgy"

[Relational Programming in miniKanren: Techniques, Applications, and Implementations](http://gradworks.umi.com/3380156.pdf) William Byrd, 2009

[Transliterating Prolog into Scheme](http://www.cs.indiana.edu/pub/techreports/TR182.pdf) Matthias Felleisen, 1985. Great, short paper, the "Prolog" fits on one page. Most my projects could be said to have been inspired by this transliterate style (into Clojure) - if only I had read it earlier.

[core.logic - A Tutorial Reconstruction](http://www.youtube.com/watch?v=A7de6pC-tnU) David Nolen, 2012

[HANSEI as a Declarative Logic Programming Language](http://okmij.org/ftp/kakuritu/logic-programming.html) Oleg Kiselyov, 2010-12

[Rule Solver: Constraint Programming with OpenRules](http://openrules.com/pdf/RulesSolver.UserManual.pdf) Jacob Feldman, 2012

* [JSR-331](http://jcp.org/aboutJava/communityprocess/final/jsr331/index.html)

[Paradigms of AI Programming](http://norvig.com/Lisp-retro.html) Peter Norvig, 1997

[Artificial Intelligence: A Modern Approach](http://aima.cs.berkeley.edu/) Stuart Russell and Peter Norvig, 2011

* "Pseudo-code algorithms from the book in [pdf](http://aima.cs.berkeley.edu/algorithms.pdf)"
* [aima-java](http://code.google.com/p/aima-java/) [Ravi Mohan](http://pindancing.blogspot.co.uk/) et al.

[Approaches to Automatic Programming](http://www.merl.com/papers/docs/TR92-04.pdf) Charles Rich and Richard C. Waters, 1992

* ["Myths and Prosopects"](http://www.isr.uci.edu/~andre/ics228s2006/richwaters.pdf) 1988, original IEEE Computer article

[Experimenting with Programming Languages](http://www.vpri.org/pdf/tr2008003_experimenting.pdf) Alessandro Warth, 2009

[PEG-based transformer provides front-, middleand back-end stages in a simple compiler](http://www.vpri.org/pdf/tr2010003_PEG.pdf) Ian Piumarta, 2010

[Instaparse](https://github.com/Engelberg/instaparse) Mark Engelberg, 2013 - "What if context-free grammars were as easy to use as regular expressions?"

[Parsing Expression Grammars](http://www.brynosaurus.com/pub/lang/peg.pdf) Brian Ford, 2004

[Packrat Parsers Can Support Left Recursion](http://www.tinlizzie.org/~awarth/papers/pepm08.pdf) Alessandro Warth et al, 2008

[Left Recursion in Parsing Expression Grammars](http://arxiv.org/pdf/1207.0443.pdf) Sergio Medeiros et al, 2012

[Scannerless Generalized-LR Parsing](file:///home/hraberg/Downloads/10.1.1.37.7828.pdf) Eelco Visser, 1997

[Generalized Parser Combinators](http://www.cs.uwm.edu/~dspiewak/papers/generalized-parser-combinators.pdf) Daniel Spiewak, 2010

[Metafor: Visualising Stories as Code](http://web.media.mit.edu/~hugo/publications/drafts/IUI2005-metafor.4.pdf) Hugo Liu and Henery Lieberman, 2005

[Subtext](http://www.subtextual.org/) [Jonathan Edwards](http://alarmingdevelopment.org), 2005 ..

[Natural Language, Semantic Analysis and Interactive Fiction](http://inform7.com/learn/documents/WhitePaper.pdf) Graham Nelson, 2005, revised 2006

[A Conceptual Overview of Prism (Languages Beyond Ada and Lisp)](http://www.dtic.mil/cgi-bin/GetTRDoc?AD=ADA240565) David Fisher and David Mundie et al, 1991 - "Our means of achieving this integrated framework is a language emphasizing expressivity and serving as a medium for dialogue, rather than one-way communication, between user and machine."

[Patterns of Software](http://www.dreamsongs.com/Files/PatternsOfSoftware.pdf) Richard Gabriel, 1996

[Aramis or the Love of Technology](http://www.bruno-latour.fr/node/106) Bruno Latour, 1996


### Notes

The letter `í` in Mímir can conveniently be typed using `C-x 8 ' i` in Emacs.


## License

Mímir, Copyright © 2012-2013 Håkan Råberg

Distributed under the Eclipse Public License, the same as Clojure.

Mimir image from [Valhalla Comics](http://www.valhalla-comics.dk) Copyright © [Peter Madsen](http://www.petermadsen.info/)

ᚠᚢᚦᚬᚱᚴ

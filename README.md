[<img width="154" src=https://github.com/hraberg/mimir/raw/master/resources/mimer.jpg alt="Mímir - Image Copyright © Peter Madsen" title="Mímir - Image Copyright © Peter Madsen" align="right" />](http://www.petermadsen.info/pages/vh/hv-er-hv/mimir.html)

# Mímir

*The god Odin carries around [Mímir's head](http://clojure.org/lazy#Making Clojure Lazier--Don't hang \(onto\) your head) and it recites secret knowledge and counsel to him.*

### Mímir is an experimental rule engine written in Clojure.

[Marginalia](http://ghettojedi.org/mimir/)

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

```clojure
  ; Rosencrantz' problem from chapter 1, "Rules to the Rescue" in Jess in Action:
  (doseq [name ["Fred" "Joe" "Bob" "Tom"]
          pants-color [:red :blue :plaid :orange]
          position (range 1 (inc 4))]
    (fact {:name name :position position :pants-color pants-color}))

  (rule find-solution
        ?g1 {:name "Fred"}

        ?g  {:position (-> ?g1 :position inc)
             :pants-color :blue}

        ?g2 {:name "Joe"
             :position 2}

        ?g3 {:name "Bob"
             :pants-color :plaid}

        ?g4 {:name "Tom"
             :position (not-in #{1 4})
             :pants-color (is-not :orange)}

        ?golfers <- #{?g1 ?g2 ?g3 ?g4}

        (different #{:position :pants-color} ?golfers)

        (contains? ?golfers ?g)

        =>

        (set ?golfers))

  (match? #{{:name "Fred", :position 1, :pants-color :orange}
            {:name "Joe", :position 2, :pants-color :blue}
            {:name "Bob", :position 4, :pants-color :plaid}
            {:name "Tom", :position 3, :pants-color :red}})
```

For more, see the [`mimir.test`](https://github.com/hraberg/mimir/tree/master/test/mimir/test).


## References

[Production Matching for Large Learning Systems](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf) Robert B. Doorenbos, 1995

[Jess in Action: Java Rule-based Systems](http://www.manning.com/friedman-hill/) Ernest Friedman-Hill, 2003

[OPS5](https://github.com/briangu/OPS5) "This Common Lisp version of OPS5 is in the public domain.  It is based in part on based on a Franz Lisp implementation done by Charles L. Forgy"

[Rule Solver: Constraint Programming with OpenRules](http://openrules.com/pdf/RulesSolver.UserManual.pdf) Jacob Feldman, 2012

* [JSR-331](http://jcp.org/aboutJava/communityprocess/final/jsr331/index.html)

[Paradigms of AI Programming](http://norvig.com/Lisp-retro.html) Peter Norvig, 1997

[Artificial Intelligence: A Modern Approach](http://aima.cs.berkeley.edu/) Stuart Russell and Peter Norvig, 2011

* "Pseudo-code algorithms from the book in [pdf](http://aima.cs.berkeley.edu/algorithms.pdf)"
* [aima-java](http://code.google.com/p/aima-java/) [Ravi Mohan](http://pindancing.blogspot.co.uk/) et al.

[Approaches to Automatic Programming](http://www.merl.com/papers/docs/TR92-04.pdf) Charles Rich and Richard C. Waters, 1992

* ["Myths and Prosopects"](http://www.isr.uci.edu/~andre/ics228s2006/richwaters.pdf) 1988, original IEEE Computer article

[Metafor: Visualising Stories as Code](http://web.media.mit.edu/~hugo/publications/drafts/IUI2005-metafor.4.pdf) Hugo Liu and Henery Lieberman, 2005

[Subtext](http://www.subtextual.org/) [Jonathan Edwards](http://alarmingdevelopment.org), 2005 ..

[Natural Language, Semantic Analysis and Interactive Fiction](http://inform7.com/learn/documents/WhitePaper.pdf) Graham Nelson, 2005, revised 2006

[Patterns of Software](http://www.dreamsongs.com/Files/PatternsOfSoftware.pdf) Richard Gabriel, 1996

[Aramis or the Love of Technology](http://www.bruno-latour.fr/node/106) Bruno Latour, 1996


### Notes

The letter `í` in Mímir can conveniently be typed using `C-x 8 ' i` in Emacs.


## License

Mímir, Copyright © 2012 Håkan Råberg

Distributed under the Eclipse Public License, the same as Clojure.

Mimir image from [Valhalla Comics](http://www.valhalla-comics.dk) Copyright © [Peter Madsen](http://www.petermadsen.info/)

ᚠᚢᚦᚬᚱᚴ

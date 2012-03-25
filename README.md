[<img width="154" src=https://github.com/hraberg/mimir/raw/master/resources/mimer.jpg alt="Mímir - Image Copyright © Peter Madsen" title="Mímir - Image Copyright © Peter Madsen" align="right" />](http://www.petermadsen.info/pages/vh/hv-er-hv/mimir.html)

# Mímir

*The god Odin carries around [Mímir's head](http://clojure.org/lazy#Making Clojure Lazier--Don't hang \(onto\) your head) and it recites secret knowledge and counsel to him.*

### Mímir is an experimental rule engine written in Clojure.

[Marginalia](http://ghettojedi.org/mimir/)

Mímir aims to implement a Rete network as a base. I don't vouch for its correctness, soundness or anything, actually. Like Mímir surely would attest, using it would be somewhat headless. Here's how it looks:

```clojure
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


## References

[Production Matching for Large Learning Systems](http://reports-archive.adm.cs.cmu.edu/anon/1995/CMU-CS-95-113.pdf), Robert B. Doorenbos, 1995

[Jess Rules in Action](http://www.manning.com/friedman-hill/) Ernest Friedman-Hill, 2003

[OPS5](https://github.com/briangu/OPS5) "This Common Lisp version of OPS5 is in the public domain.  It is based in part on based on a Franz Lisp implementation done by Charles L. Forgy"

### Notes

The letter `í` in Mímir can conveniently be typed using `C-x 8 ' i` in Emacs.


## License

Mímir, Copyright © 2012 Håkan Råberg

Distributed under the Eclipse Public License, the same as Clojure.

Mimir image from [Valhalla Comics](www.valhalla-comics.dk) Copyright © [Peter Madsen](http://www.petermadsen.info/)

ᚠᚢᚦᚬᚱᚴ

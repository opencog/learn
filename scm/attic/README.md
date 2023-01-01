Attic
-----
Dead, unused code that might still be interesting. A museum of old
stuff.

* __lg-export__: Export AtomSpace contents into Link Grammar SQLite3
    dictionaries. It "works" but is pointless; it's just an extra
    step of complexity that is not needed.

* __cluster__: Assorted attempts at clustering words into clusters.
    None worked out, due to complexity, bugs, mis-design,
    mis-understanding what the actual problem was.

* __mst-parse__: The older, scheme-based MST parser built with the
    opencog sheaf code. The LG parser can now do MST and MPG parsing,
    and so provides a better platform. So, this older parser is now
    obsolete. BTW, so is the sheaf code.

* __pair-count__: older pair-counting code. It tried to do too much,
    was too general. Thus, too complex. We now know exacly how to do
    pair-counting: simply and directly. The older proof-of-concept
    code used this.

* __fibers-sim.scm__: Attempt to use scheme/guile fibers to obtain
    parallelism in the similarity calculations. Fails to improve
    performance. Plus fibers are buggy, somehow.

* __cheesy-thread.scm__: Another attempt at parallelizing similarity
    calculations.
 
Stuff below should be deleted a few years after being declared dead.

* __singletons.scm__: Code for working with single words.  Not used
    anywhere. Well, used in the summary reports below.

* __summary.scm__: Stale, boring report printer. The reports are anemic.
    Other than that, they do work.

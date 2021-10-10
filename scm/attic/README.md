Dead, unused code that might still be interesting.

Stuff here should be deleted a few years after being declared dead.

* __gram-fuzz.scm__ : Assorted styles of doing fuzzy union merging.
    The idea here was that when merging two words, one would always
    merge those disjuncts they had in common, and move a fraction of
    the counts of the rest. This is a kind-of fuzzy-set-like concept,
    and the fuzziness did nothing for the merge quality.  I dunno,
    it might be OK, but really not very inspiring, in the end.

* __shape-project.scm__: Failed attempt to merge shapes.  Death by complexity
    and bugginess.

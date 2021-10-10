Dead, unused code that might still be interesting.

Stuff here should be deleted a few years after being declared dead.

* __gram-fuzz.scm__ : Assorted styles of doing fuzzy union merging.
    The idea here was that when merging two words, one would always
    merge those disjuncts they had in common, and move a fraction of
    the counts of the rest. This is a kind-of fuzzy-set-like concept,
    and the fuzziness did nothing for the merge quality.  I dunno,
    it might be OK, but really not very inspiring, in the end.

* __agglo-loops.scm__ : Top-level routines to do pair-wise clustering.
    The code works, but doing pair-wise clustering seems not to give
    the best results.  This file implements thee different algos that
    assign words to existing clustters, or if there aren't any suitable
    ones, they create a new cluster. Again, this is OK, the code works,
    it gives OK results, but the newer clustering code in
    __agglorank.scm__ is better.

* __shape-project.scm__: Failed attempt to merge shapes.  Death by complexity
    and bugginess.

Dead, unused code that might still be interesting.

Stuff here should be deleted a few years after being declared dead.

* __gram-fuzz.scm__ : Assorted styles of doing fuzzy union merging.
    The idea here was that when merging two words, one would always
    merge those disjuncts they had in common, and move a fraction of
    the counts of the rest. This is a kind-of fuzzy-set-like concept,
    and the fuzziness did nothing for the merge quality.  I dunno,
    it might be OK, but really not very inspiring, in the end.
    This is kept here for now, because it was explored extensively
    in the Diary Part One

* __agglo-loops.scm__ : Top-level routines to do pair-wise clustering.
    The code works, but doing pair-wise clustering seems not to give
    the best results.  This file implements thee different algos that
    assign words to existing clusters, or if there aren't any suitable
    ones, they create a new cluster. This is OK, the code works, it
    gives OK results. It is kept here because it was explored and
    characterized extensively in Diary Part One. The newer clustering
    code in __agglo-rank.scm__ is better. The newer code is explored
    and characterized in Diary Part Four.

* __singletons.scm__: Code for working with single words.  Not used
    anywhere. Well, used in the summary reports below.

* __summary.scm__: Stale, boring report printer. The reports are anemic.
    Other than that, they do work.

* __shape-project.scm__, __cset-class.scm__, __cset-merge.scm__: Failed
    attempt to merge shapes.  Death by complexity and bugginess. The
    variant that does work is in __shape-vec.scm__ in the directory
    above.

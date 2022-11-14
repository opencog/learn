Dead, unused code that might still be interesting.

* __link-class.scm__: Desribes a form of link discovery and
    classification that was never implemened. The 'shapes' idea took
    its place. Still, it seems odd and curious; maybe something can
    be done with this idea?

* __gram-pairwise.scm__ : Code that performs pairwise clustering.
    (clustering two words, with a vector of disjuncts behind them.)
    Three basic functions are provided: (a) merge two words into a
    new cluster. (b) merge one word into an existing cluster.
    (c) merge two clusters together. This code has been supplanted
    by `gram-majority.scm` which merged N words into a new cluster.
    The `gram-majority` code also handles cases (b) and (c) but
    these two cases are not currently needed for anything, except
    for the unit tests. So maybe ... I dunno, case (b) and (c) might
    need to be revived from the code here?  Anyway, this code does
    work, and it worked well, and is fully debugged and "bulletproof"
    and was passing all unit tests when it was retired. So its good
    code, its just ... the majority style seems like a better idea.

* __agglo-pairwise.scm__ : Main loop for above. Makes calls to above.
    The code won't stand alone here, it needs helper routines defined
    in `agglo--rank.scm`.

* __gram-fuzz.scm__ : Assorted styles of doing fuzzy union merging.
    The idea here was that when merging two words, one would always
    merge those disjuncts they had in common, and move a fraction of
    the counts of the rest. This is a kind-of fuzzy-set-like concept,
    and the fuzziness did nothing for the merge quality.  I dunno,
    it might be OK, but really not very inspiring, in the end.
    This is kept here for now, because it was explored extensively
    in the Diary Part One.

* __agglo-loops.scm__ : Top-level routines to do pair-wise clustering.
    (Clustering two words, with a vector of disjuncts behind them.)
    The code works, but doing pair-wise clustering seems not to give
    the best results.  This file implements there different algos that
    assign words to existing clusters, or, if there aren't any suitable
    ones, they create a new cluster. This is OK, the code works, it
    gives OK results. It is kept here because it was explored and
    characterized extensively in Diary Part One. The newer clustering
    code in __agglo-rank.scm__ is better. The newer code is explored
    and characterized in Diary Part Four.

Stuff below should be deleted a few years after being declared dead.

* __shape-project.scm__, __cset-class.scm__, __cset-merge.scm__: Failed
    attempt to merge shapes.  Death by complexity and bugginess. The
    variant that does work is in __shape-vec.scm__ in the directory
    above.

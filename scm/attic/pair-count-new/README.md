Counting of Pairs
-----------------
The code here provides functions for counting word pairs occurring in
natural language text.  The primary goal is to avoid any assumptions
about the locations of sentence boundaries. It does assume that words
can be split according to white-space (this is mildly configurable).
The sampling is uniform over parse trees (and NOT uniform over pairs!)
It seems that uniform sampling over trees should be superior.

Deprecated: this implements batch-process counting, using the `matrix` API.
But I'm trying to move away from batch processing, so this is all slowly
being retired.

### Quick overview.

* __clique-pair-count.scm__ -- Perform naive clique counting. Currently
    unused (mostly because it's naive.)

* __word-pair-count.scm__ -- Create and return a function that takes,
    as input, a (fairly short) text string, performs a random planar
    parse on it, and updates the counts on the resulting word-pairs.

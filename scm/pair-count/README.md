Counting of Pairs
-----------------
The code here provides functions for counting word pairs occuring in
natural langauge text.  The primary goal is to avoid any assumptions
about the locations of sentence boundaries. It does assume that words
can be split according to white-space (this is mildly configurable).
The sampling is uniform over parse trees (and NOT uniform over pairs!)
It seems that uniform sampling over trees should be superior.

###Quick overview.

* __pair-api.scm__ -- Defines the word-pair API matrix object. This
    object defines how word-pairs are stored in the AtomSpace.

* __word-pair-count.scm__ -- Create and return a function that takes,
    as input, a (fairly short) text string, performs a random planar
    parse on it, and updates the counts on the resulting word-pairs.

* __sliding-block.scm__ -- Define a function that takes a large (huge)
    text string block, and scans it with a rather smaller sliding
    window.

* __pair-count-window.scm__ -- Perform naive clique counting. Currently
    unused (mostly because it's naive.)

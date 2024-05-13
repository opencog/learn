Counting of Pairs
-----------------
The code here provides functions for counting word pairs occurring in
natural language text.  The primary goal is to avoid any assumptions
about the locations of sentence boundaries. It does assume that words
can be split according to white-space (this is mildly configurable).
The sampling is uniform over parse trees (and NOT uniform over pairs!)
It seems that uniform sampling over trees should be superior.

### Quick overview.

* __pair-api.scm__ -- Defines the word-pair API matrix object. This
    object defines how word-pairs are stored in the AtomSpace.

* __word-pair-pipe.scm__ -- Create and return a function that takes,
    as input, a (fairly short) text string, performs a random planar
    parse on it, and updates the counts on the resulting word-pairs.
    Uses an Atomese pipe to do the counting. This is over 3x faster
    than the older code (now moved to
    `attic/pair-count-new/word-pair-count.scm`)
    This new avoids lock thrashing, and scheme garbage-collection.
    It does not use the old matrix API.  This provides a
    function-compatible replacement for the earlier pair-counting API.
    Works great!

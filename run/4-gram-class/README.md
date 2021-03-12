
Agglomerative-clustering management scripts
===========================================
See `../../README-Natural.md` for more info.

A quick overview:

* `dict-comp.scm` -- Perform validation of dictionary. Takes two
  arguments: the dictironary name, and a file containing sentences.
  It reads the sentence file, compares them to parses from the
  default Link Grammar English dictionary, and tabulates results.
  Usage: `guile -s dict-comp.scm <dict-name> <sentence-file-name>`

* `test-data` -- Directory containing evaluation corpora. These
  can be used to measure parser covereage, and to compare the operation
  of different parsers.

* `run-gram-cogserver.sh`: Starts a guile shell, runs the atomspace,
  starts the cogserver in the background, and opens the storage
  database, and loads the disjuncts from the database.

* `run-all-gram.sh`: combines all of the above steps into one.

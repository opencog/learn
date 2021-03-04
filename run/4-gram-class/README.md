
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

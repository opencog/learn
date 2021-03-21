
Result evaluation scripts
=========================
See `../../README-Natural.md` for more info.

A quick overview:

* `en-dict-comp.scm` -- Perform validation of a dictionary against
  the Link Grammar English dictionary. Takes two arguments: the
  dictionary name, and a file containing sentences.  It reads the
  sentence file, compares them to parses from the default English
  dictionary, and tabulates results.
  Usage: `guile -s en-dict-comp.scm <dict-name> <sentence-file-name>`

* `en-test-data` -- Directory containing evaluation corpora. These
  can be used to measure parser covereage, and to compare the operation
  of different parsers.

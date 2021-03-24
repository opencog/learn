
Result evaluation scripts
=========================
See `../../README-Natural.md` for more info.

These are currently ad hoc and unpolished. It is still not clear how
to best evaluate results.

A quick overview:

* `dict-comp.scm` -- Validate a test dictionary against a golden
  dictionary, using a corpus of sentences. Takes three arguments:
  the golden dictionary, the test dictionary, and the corpus of
  sentences that the two should be evaluated on.
  Example usage:
  `./dict-comp.scm fake-lang learned fake-corpus/corpus-5.txt`

* `en-dict-comp.scm` -- Perform validation of a dictionary against
  the Link Grammar English dictionary. Takes two arguments: the
  dictionary name, and a file containing sentences.  It reads the
  sentence file, compares them to parses from the default English
  dictionary, and tabulates results.
  Usage: `guile -s en-dict-comp.scm <dict-name> <sentence-file-name>`

* `en-test-data` -- Directory containing evaluation corpora. These
  can be used to measure parser covereage, and to compare the operation
  of different parsers.

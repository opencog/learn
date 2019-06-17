
Agglomerative-clustering management scripts
===========================================

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the third stage of the language learning pipeline.
This stage clusters words into grammatical categories. It assumes
that the second processing step has been completed, and that a
database is available with a large number of disjunct counts,
together with the marginals for them.

You will typically want to make copies of these, and tailor them to
your specific needs and procedures. In particular, many of these
files require database credentials to be set; the exact credentials
to use will depend on which copy of which database you are using.
You WILL be copying around a lot of databases!

A quick overview:

* `dict-comp.scm` -- Perform validation of dictionary. Takes two
  arguments: the dictironary name, and a file containing sentences.
  It reads the sentence file, compares them to parses from the
  default Link Grammar English dictionary, and tabulates results.
  Usage: `guile -s dict-comp.scm <dict-name> <sentence-file-name>`

* `test-data` -- Directory containing evaluation datasets. Currently
  contains `child-directed-speech.txt`, a collection of over 3000
  short sentences spoken by adults to children.

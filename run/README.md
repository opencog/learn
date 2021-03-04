
Parse management scripts
========================

The scripts here are used to automate the operation of the
language-learning pipeline. Currently, it consists of five or six steps:

* Overall pipeline configuration files. Located in
  [0-config](0-config). These should be reviewed and edited to suit.

* Artificial grammar generation. Automation scripts can be found in the
  [1-gen-dict](1-gen-dict) directory. This is required for calibration
  of the learning system. Just run `gen-corpus.sh` in this directory.

* For natural language learning, a text corpus is needed. Any
  sufficiently large collection of plain-text UTF-8 files will do.
  Some tools for downloading Wikipedia and Project Gutenberg texts
  can be found in the `../download` directory.

* Word-pair counting. Automation scripts can be found in the
  [2-word-pairs](2-word-pairs) directory.

* MST parsing. Automation scripts can be found in the
  [3-mst-parsing](3-mst-parsing) directory. The previous step must
  have been completed, before starting this.

* Grammatical class learning. Automation scripts can be found in the
  [4-gram-class](4-gram-class) directory. The previous step must
  have been completed, before starting this.

* Comparison of the learned grammar to the input grammar in step 0.
  TBD.

The file `wikipedia.txt` contains multiple wikipedia artices, and can be
used for trial runs.

Other things
-------------
* [common](common) -- scripts that are common to several processing
  steps.

* [attic](attic) -- obsolete code that might still be useful.

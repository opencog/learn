
Parse management scripts
========================

Under reconstruction.  These scripts are being redesigned *right now*.
The documentation may be contradictory or incoherent. For an older working
version, see the [attic](../attic/run-v3).

The scripts here are used to automate the operation of the
language-learning pipeline. Currently, it consists of five or six
semi-automated steps. These steps are "semi-automated", in that
they are meant to be run by hand, by running the shell scripts
in each directory.

Full automation is provided by docker containers. These are found in
the [opencog/docker](https://github.com/opencog/docker) git repo.
These are designed so that you can just drop off your input text into
designated diectories, and then just run the containers. The resulting
datasets are copied out of the docker containers, where you can get at
them. This simplifies the processing of different kinds of texts.

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

* Comparison of the learned grammar to the input grammar. These are
  the datasets from the 2019 ULL-POC work.
  Incomplete. Some work in [5-compare](5-compare).

* All-in-one. The `all-in-one.sh` file combines steps 2, 3 and 4 in
  a completely automated way.


The file `wikipedia.txt` contains multiple wikipedia artices, and can be
used for trial runs.


MST-Parsing management scripts
==============================
Maximum Spanning Tree Parsing. The parser invoked in these scripts
performs an MST parse. This parse may result in non-planar trees;
the scripts in the `2-mpg-parsing` directory invoke a planar MST
parser.

TODO: the MPG scripts are very nearly identical to the scripts here;
they should be merged, so that planar/non-planar is a configurable
flag.

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the second stage, (the MST-parsing stage) of the
language learning pipeline. This stage assumes that word-pair counting
has been performed, and that marginals and the mutual information (MI)
for word pairs has been computed and stored in a database.

You will typically want to make copies of these scripts, and tailor
them to your specific needs and procedures. In particular, many of
these files require database credentials to be set; the exact
credentials to use will depend on which copy of which database you
are using.  You WILL be copying around a lot of databases!

A quick overview:

* `run-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals.

* `mst-submit.sh`: MST processing script.
  This pulls text files, one by one, from the data directory, and
  submits them for MST processing. Run this manually in the `submit`
  tmux/byobu window.  Be sure to have performed the word-pair mutual
  information step first. Be sure to make a copy of your database.
  Be sure to open the database, first.

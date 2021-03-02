
MST-Parsing management scripts
==============================

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

* `mst-submit-??.sh`: language-specific MST processing scripts.
  These pull text files, one by one, from the data directory, and
  submit them for MST processing. Pick one, and run it manually
  the 'submit' byobu window.  Be sure to have performed the mutual
  information step first. Be sure to make a copy of your database.
  Be sure to open the database, first.

  The directory containing the text files needs to be manually adjusted
  here; its `gamma-pages` by default, but you can use any directory
  that you wish.

* `mst-one.sh`: processes files, one at a time. Performs sentence
   splitting on the provided file, using the provided language encoding.

* `mst-nosplit-one.sh`: processes files, one at a time. Assumes one
   sentence per line.

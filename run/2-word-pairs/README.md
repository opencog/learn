
Word-pair counting management scripts
=====================================

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the first stage of the language learning pipeline.
These can be applied to any flat text files from any origin of your
choice.  Some tools for downloading Wikipedia and Project Gutenberg
texts can be found in the `../../download` directory.  Tools for
generating artificial langauges are in the
[`../1-gen-dict`](../1-gen-dict) directory.

You will typically want to make copies of these scripts, and tailor
them to your specific needs and procedures. In particular, many of
these files require database credentials to be set; the exact
credentials to use will depend on which copy of which database you
are using.  You WILL be copying around a lot of databases!

Detailed istructions on what to do can be found in
[`../../README-Natural.md`](../../README-Natural.md)

File overview
-------------
A quick overview:

* `run-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals.

* `pair-submit-??.sh`: language-specific word-pair-counting scripts.
  These pull text files, one by one, from the data directory, and
  submit them for word-pair counting. Pick one, and run it manually
  the 'submit' byobu window.  Be sure to open the database, first.
  The directory containing the text files needs to be manually adjusted
  here; its `beta-pages` by default, but you can use any directory
  that you wish.

* `pair-one.sh`: the actual sentence-splitting workhorse. It handles each
  text file, moving the file to a different directory when finished
  with it.  Note that there are hard-coded paths in here, pointing to
  the sentence splitter.

* `pair-nosplit-one.sh`: similar to above, but assumes that the
  text-file contains one sentence per line - i.e. has been pre-split.

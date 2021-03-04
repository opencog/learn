
Word-pair counting management scripts
=====================================

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the first stage of the language learning pipeline.
These can be applied to any flat text files from any origin of your
choice.  Some tools for downloading Wikipedia and Project Gutenberg
texts can be found in the `../../download` directory.  Tools for
generating artificial languages are in the
[`../1-gen-dict`](../1-gen-dict) directory.

Because the text processing can take a long time to run (hours or days)
the general process is hard to automate, and works best if it is
monitored for forward progress. This is done by starting a cogserver
in one terminal, and sending text to it in another.

The main entry point here is `run-shells.sh` which creates multiple
terminal sessions in tmux/byobu. One terminal will have the cogserver
running in it, and another will have the text-feeder script. By default,
the text-feeder script is not started automatically; toggle through
the terminals with F3 and F4 for hints, or look at `run-shells.sh`
directly.

The scripts here assume that `../0-config/0-pipeline.sh` and that
`../0-config/2-pair-conf.sh` have been configured.

Detailed instructions on what to do can be found in
[`../../README-Natural.md`](../../README-Natural.md)

File overview
-------------
A quick overview:

* `run-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals.

* `pair-submit.sh`: feeds text to the cogserver for word-pair counting.
  This pull text files, one by one, from the data directory, and submits
  them for word-pair counting. Start it manually in the `submit` byobu
  window. The cogserver needs to be running first, and a database needs
  to be open (it should be in another byobu terminal).

* `pair-split-one.sh`: helper script. This is used when the text appears
  in conventional paragraphs; it will split the paragraphs into
  individual sentences.  It handles each text file, moving the file to
  a different directory when finished with it.  Note that there are
  hard-coded paths in here, pointing to the sentence splitter.

* `pair-nosplit-one.sh`: similar to above, but assumes that the
  text-file contains one sentence per line - i.e. has been pre-split.

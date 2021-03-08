
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

After counting has completed, marginal statistics must be computed.
This can be done by hand, by running `pair-marginals.scm`.

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

After this stage is completed, move on to `../3-mst-parsing`.

File overview
-------------
A quick overview:

* `run-cogserver.sh`: Starts a guile shell, runs the atomspace, starts
  the cogserver in the background, and opens the storage database.

* `run-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals.

* `pair-submit.sh`: feeds text to the cogserver for word-pair counting.
  This pull text files, one by one, from the data directory, and submits
  them for word-pair counting. Start it manually in the `submit` byobu
  window. The cogserver needs to be running first, and a database needs
  to be open (it should be in another byobu terminal).

* `compute-marginals.sh`: A bash script that computes marginal statistics
  after pair counting has concluded. This needs to be run by hand.

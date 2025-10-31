
Word-pair counting management scripts
=====================================

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the first stage of the language learning pipeline.
These can be applied to any flat text files from any origin of your
choice.  Some tools for downloading Wikipedia and Project Gutenberg
texts can be found in the `../../download` directory.  Tools for
generating artificial languages are in the
[`../1-gen-dict`](../1-gen-dict) directory.

Processing can be done in a "fully automated" way or in a semi-automated
way. For full automation, just run `run-all.sh` and move to the next
stage (stage `3-mst-parsing`). However...

Text processing a large corpus can take a long time to run (hours or
days) and if issues arise, it may be easier to run in a semi-automated
mode.  This is done by starting a cogserver in one terminal with
`run-cogserver.sh`, and sending text to it in another, with
`pair-submit.sh`. If counting is interrupted, just re-run
`pair-submit.sh` again, and it will pick up hwere it left off.

After counting has completed, marginal statistics must be computed.
This can be done by hand, by running `compute-marginals.sh`.

The easiest way to run in semi-automated mode is to run the
`run-shells.sh` script.  It creates multiple terminal sessions in
tmux/byobu. One terminal will have the cogserver running in it, and
another will have the text-feeder script. By default, the text-feeder
script is not started automatically; toggle through the terminals
with F3 and F4 for hints, or just look at `run-shells.sh` directly.

The scripts here assume that `run-config/0-pipeline.sh` and that
`run-config/2-pair-conf.sh` have been configured.

Detailed instructions on what to do can be found in
[`../../README-Natural.md`](../../README-Natural.md)

After this stage is completed, move on to `../3-mst-parsing`.

File overview
-------------
A quick overview:

* `run-cogserver.sh`: Starts a guile shell, runs the atomspace, starts
  the cogserver in the background, and opens the storage database.

* `pair-submit.sh`: feeds text to the cogserver for word-pair counting.
  This pull text files, one by one, from the data directory, and submits
  them for word-pair counting. Start it manually in the `submit` byobu
  window. The cogserver needs to be running first, and a database needs
  to be open (it should be in another byobu terminal).

* `compute-marginals.sh`: A bash script that computes marginal statistics
  after pair counting has concluded. This needs to be run by hand.

* `run-all.sh`: Combine all of the above into one step. When this
  finishes, move to step `3-mst-parsing`.

* `run-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals. Switch
  to the "submit" terminal, and start `pair-submit.sh` by hand.

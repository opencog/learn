
MST-Parsing management scripts
==============================
Maximum Spanning Tree Parsing. The parser invoked in these scripts
performs an MST parse.

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the third stage, (the MST-parsing stage) of the
language learning pipeline. This stage assumes that word-pair counting
has been performed, and that marginals and the mutual information (MI)
for word pairs has been computed and stored in a database.

Adjust the configuration in `../0-config` before running anything here.

This step can be run in a fully-automated way, by invoking
`run-all-mst.sh` and then moving to the next step: `4-gram-class`.

However ... if any issues arise, this step can be run in a semi-automated
way. The cogserver can be started with `run-mst-cogserver.sh`. Be sure
to copy the database, first. (Copying the pairs database to a distinct
MST database provides isolation and protection against data corruption,
in case anything goes wrong.)

Once the cogserver has started, run `mst-submit.sh` to process the
corpus. If processing is interrupted, it can be restarted; processing
will resume where it left off.

Once processing is complete, run `compute-mst-marginals.sh` as the last
step before moving to stage four (`4-gram-class`).

A quick overview:

* `run-mst-cogserver.sh`: Starts a guile shell, runs the atomspace,
  starts the cogserver in the background, and opens the storage
  database, and loads the word-pairs from the database.

* `run-mst-shells.sh`: multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the cogserver in one
  of them.  Use F3 and F4 to switch to different terminals.

* `mst-submit.sh`: MST processing script.
  This pulls text files, one by one, from the data directory, and
  submits them for MST processing. Run this manually in the `submit`
  tmux/byobu window.

* `compute-mst-marginals.sh`: A bash script that computes marginal
  statistics after MST parsing has concluded. This needs to be run
  by hand.

* `run-all.sh`: combines all of the above steps into one.

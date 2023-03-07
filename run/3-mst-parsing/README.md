
MST-Parsing management scripts
==============================
Maximum Spanning Tree Parsing. The parser invoked in these scripts
performs an MST parse.

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the third stage, (the MST-parsing stage) of the
language learning pipeline. This stage assumes that word-pair counting
has been performed, and that marginals and the mutual information (MI)
for word pairs has been computed and stored in a database.

Running
-------
Review and adjust the configuration in `run-config/0-pipeline.sh` and
`run-config/3-mpg-conf.sh` before running anything here.

MST parsing can be run in a fully-automated way, by invoking
`run-all-mst.sh`. Once this completes, move to the next step:
`4-gram-class`.

Full automation assumes you've configured everything correctly, that
the text files and datasets are where they should be, and that the
code-base is bug-free.

If any issues arise, this step can be run in a semi-automated way.
The CogServer can be started with `./run-mst-cogserver.sh`. Be sure
to copy the database, first. (Copying the pairs database to a distinct
MST database provides isolation and protection against data corruption,
in case anything goes wrong.)

Once the CogServer has started, run `./mst-submit.sh` to process the
corpus. If processing is interrupted, it can be restarted; processing
will resume where it left off.

Once processing is complete, run `./compute-mst-marginals.sh` as the last
step before moving to stage four (`4-gram-class`).

Use the `./run-mst-shells.sh` script to open a byobu/tmux session,
with panels already set up for the processing steps above.

File overview
-------------
A quick overview:

* `run-mst-cogserver.sh`: Starts a guile shell, starts the CogServer,
  and opens the configured storage database, and loads the word-pairs
  from the database.

* `run-mst-shells.sh`: Multi-tasking terminal server.  Opens multiple
  terminal sessions with tmux/byobu, and starts the CogServer in one
  of them.  Use F3 and F4 to switch to different terminals.

* `mst-submit.sh`: MST processing script.
  This pulls text files, one by one, from the text directory, and
  submits them for MST processing. Run this manually in the `submit`
  tmux/byobu window.

* `compute-mst-marginals.sh`: A bash script that computes marginal
  statistics after MST parsing has concluded. This needs to be run
  by hand.

* `run-all-mst.sh`: combines all of the above steps into one.

Automation notes
----------------
Some notes about the automation process.

* Processing of texts is held off until all word-pairs are loaded. This
  is done with "gates" -- a mutex, called `mst-gate` is created. It is
  locked during word-pair loading, and unlocked when done. The text
  observers are blocked, waiting for this mutex.

* When the submission of texts for counting has ended, the submit script
  calls `(finish-mst-submit)`.  This can be re-defined to do anything,
  when called. By default its a no-op. The fully-automated scripts define
  it so that the CogServer exits.

* When marginals compution finishes, the `/tmp/mst-marginals-done` file
  is touched. Docker uses this to self-halt the container that's running.
  Yes, this is a non-unique tag, if not running in docker, and it needs
  fixing.

---------

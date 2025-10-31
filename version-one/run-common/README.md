
Parse management scripts
========================

The scripts here are used to automate the operation of the
language-learning pipeline. This directory contains scripts shared
by several different steps.

File overview
-------------
Several files common to several of these steps are located in this
directory.  A quick overview:

* `submit-block.pl`: Script to send UTF-8 text from STDIN, as a single
  large block, to the cogserver.  Performs only one submit for the
  entire text input, as one big blob.
  Used for pair-counting, MST-parsing and MPG-parsing.

* `submit-lines.pl`: Script to send UTF-8 text from STDIN to the
  cogserver, line by line.
  Used for pair-counting, MST-parsing and MPG-parsing.

* `socket-send.pl`: Network interface to the cogserver. Used by above.

* `file-block-process.sh`: Submit each text file for processing,
  moving the file to a temp directory while it is being processed,
  and then a final directory when the processing is completed.
  This submits the entire file as one single big block of text.

* `file-line-process.sh`: Submit each text file for processing,
  one line at a time. That is, newlines are assumed to delimit blocks
  of text; each of these is submitted individually to the cogserver.

* `file-xform-process.sh`: similar to above, but does not assume
  that the corpus file is text; rather, it applies a user-specified
  command to transform the input file into text, and then delivers
  that text, one line at a time, to the cogserver.

* `cogserver.scm`: Guile scheme script to load needed guile modules,
  start the cogserver, and open the database storage backend.

* `marginals-pair.scm`: Guile scheme script to compute word-pair
  marginal statistics.

* `marginals-mst.scm`: Guile scheme script to compute disjunct
  marginal statistics.

* `cogserver-mst.scm`: Same as `cogserver.scm`, but also loads
  word-pairs from storage. (Word pairs must be in RAM in order for
  MST parsing to proceed.)

* `cogserver-gram.scm`: Same as `cogserver.scm`, but also loads
  disjuncts from storage, and creates cross-sections. (Disjuncts
  must be in RAM in order for grammatical classification to proceed.)

* `cogserver-lg.scm`: Same as `cogserver.scm`, but also loads
  disjuncts from storage. Does not create cross-sections. (Disjuncts
  must be in RAM in order for the LG parser to be able to access them.)

Management scripts
------------------
Several files are used for overall process management:

* `renice.sh`: Make the Postgres server run under a nice priority.

* `halt-all.sh`: Stop all running LXC containers.

Notes about the CogServer
-------------------------
The CogServer is used only because the conventional guile REPL server
is not sufficiently stable to be usable in production. It is also rather
slow; the CogServer is roughly 4x faster.

If it weren't for these issues, you could get functionality more-or-less
equivalent to the CogServer in pure guile, by running the following:
```
(use-modules (system repl common))
(use-modules (system repl server))

; Write a log-file, just in case...
(cog-logger-set-filename! "/tmp/mpg-en.log")
(cog-logger-info "Start MPG parsing for English.")

; Start the network REPL server on port 19005
(call-with-new-thread (lambda ()
   (repl-default-option-set! 'prompt "scheme@(en-mpg)> ")
   (set-current-error-port (%make-void-port "w"))
   (run-server (make-tcp-server-socket #:port 19005)))
)
```
For short runs, the above does behave more or less the same way as the
CogServer does. Unfortunately, it crashes/hangs after about half and
hour (or sooner???), under a heavy load.

Deprecated scripts
------------------
The following scripts and directories are no longer recommended, and
will be removed someday. They supported text segmentation into sentences
via perl scripts. This is problematic: the language pipeline itself
needs to be able to segment as needed. These scripts were used during
bringup, but are not uased any more.

* `split-sentences.pl`: Split text files into sentences. Accepts
  free-form text, and looks for language-depedeny likely end-of
  sentence locations, so that there is one sentence per line.
  It's language-dependent, in order to not confuse abbreviations
  with end-of-sentence markers.

* `nonbreaking_prefixes` Used by the sentence-splitter to avoid
  breaking on abbreviations.

* `file-split-process.sh`: helper script. This is used when the text
  appears in conventional paragraphs; it will split the paragraphs into
  individual sentences.  It handles each text file, moving the file to
  a different directory when finished with it.  Note that there are
  hard-coded paths in here, pointing to the sentence splitter.


Sentence Splitting
------------------
Raw text needs to be split up into sentences.  During bringup, this was
done with some perl scripts that know about per-language sentence
segmentation. This is described below. It is deprecated and will be
removed someday.

Splitting is done with the `split-sentences.pl` perl script in the this
directory.  It was stolen from the `moses-smt` package.
https://github.com/moses-smt/mosesdecoder/tree/master/scripts/share/nonbreaking_prefixes
It splits French, Polish, Lithuanian, and more.  Its LGPL.

You can verify that it works, like so:
```
   cat text-file | ./split-sentences.pl -l en > x
```
Replace `en` by the language of your choice.

Some typical sentence-splitting concerns that the above script seems
to mostly handle correctly:

A question mark or exclamation mark always ends a sentence.  A period
followed by an upper-case letter generally ends a sentence, but there
are a number of exceptions.  For example, if the period is part of an
abbreviated title ("Mr.", "Gen.", ...), it does not end a sentence.
A period following a single capitalized letter is assumed to be a
person's initial, and is not considered the end of a sentence.

Attic
-----
Obsolete code that might still be useful.


Parse management scripts
========================

The scripts here are used to automate the operation of the
language-learning pipeline. This directory contains scripts shared
by several different steps.

File overview
-------------
Several files common to several of these steps are located in this
directory.  A quick overview:

* `gen-dict.scm` -- Generates a random artificial ("fake") grammar.
  Requires configuration parameters to be declared; see
  `../0-config/dict-conf.scm` for an example configuration file.

   Once the parameters are configured as desired, the grammar can
   be generated as
```
   $ ./gen-dict.scm dict-conf.scm
```

* `fake-lang` -- Link Grammar boilerplate, required for working with
   a Link-Grammar file-based dictionary. The `gen-dict.scm` file copies
   these files to the target dictionary location. For example:
```
   $ cp -r fake-lang /home/ubuntu/data/trial-run/fake-lang
```

* `split-sentences.pl`: Split text files into sentences. Accepts
  free-form text, and looks for language-depedeny likely end-of
  sentence locations, so that there is one sentence per line.
  It's language-dependent, in order to not confuse abbreviations
  with end-of-sentence markers.

* `nonbreaking_prefixes` Used by the sentence-splitter to avoid
  breaking on abbreviations.

* `submit-plain.pl`: Script to send text strings to the cogserver.
  Used for pair-counting, MST-parsing and MPG-parsing.

* `submit-one.pl`: Script to send single sentences to the cogserver.
  Similar to above, but strips out stray HTML markup, and comment
  cards.  Used for pair-counting, MST-parsing and MPG-parsing.

* `socket-send.pl`: Network interface to the cogserver. Used by above.

* `file-split-process.sh`: helper script. This is used when the text
  appears in conventional paragraphs; it will split the paragraphs into
  individual sentences.  It handles each text file, moving the file to
  a different directory when finished with it.  Note that there are
  hard-coded paths in here, pointing to the sentence splitter.

* `file-nosplit-process.sh`: similar to above, but assumes that the
  text-file contains one sentence per line - i.e. has been pre-split.

* `file-xform-process.sh`: similar to above, but does not assume
  that the corpus file is text; rather, it applies a user-specified
  command to transform the input file into text, and then delivers
  that text to the cogserver.

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

* `export-dictionary.scm`: Export the result of classification as
  a Link Grammar-compatible dictionary.

* `cogserver-lg.scm`: Same as `cogserver.scm`, but also loads
  disjuncts from storage. Does not create cross-sections. (Disjuncts
  must be in RAM in order for the LG parser to be able to access them.)


Management scripts
------------------
Several files are used for overall process management:

* `renice.sh`: Make the Postgres server run under a nice priority.

* `halt-all.sh`: Stop all running LXC containers.

Sentence Splitting
------------------
Raw text needs to be split up into sentences.  Some distant future day,
opencog will do this automatically. For now, we hack it.

Currently, splitting is done with the `split-sentences.pl` perl script
in the this directory.  It was stolen from the `moses-smt` package.
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

Attic
-----
Obsolete code that might still be useful.

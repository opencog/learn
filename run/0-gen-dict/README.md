
Artificial grammars and corpora
===============================

Edit `gen-dict.scm` and adjust parameters as desired.

Edit `gen-corpus.sh` and set directory path to where you want the
generated corpus to go. Then run it.

Move to step `../run/1-word-pairs` next.

File overview
-------------
* `fake-lang` -- Link Grammar boilerplate, required for working with
   a Link-Grammar file-based dictionary. Copy this to whichever directory
   you and to use to hold the generated grammar. For example:
```
   $ cp -r fake-lang /home/ubuntu/data/trial-run/fake-lang
```

* `gen-dict.scm` -- This contains the configurable parameters for
   generating a random artificial ("fake") grammar. Once these are
   configured as desired, the grammar can be generated as
```
   $ ./gen-dict.scm
```

* `gen-corpus.sh` -- This generates a random corpus from the fake
   grammar. Edit and configure as desired. This automatically invokes
   `gen-dict.scm` to crete the dictionary. The location of the
   dictionary, and the corpus, is configured in `gen-dict.scm`.

Notes
-----
Currently, corpora are generated using the `link-generator` tool from
a custom modified link-grammar tool. (See ../../README-Calibration.md
for details.) It can be used as follows:
```
$ cp -r fake-lang /tmp/fake-lang
$ link-generator -l /tmp/fake-lang s 6 -c 50000 > /tmp/corpus.txt
```
This will generate 50000 random sentences that are 6 words long.
If the grammar does not allow this many sentences to be generated,
fewer will be created. If the grammar allows more than 50000 sentences,
then a random sampling of 50000 sentences will be made.

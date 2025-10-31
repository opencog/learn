
Artificial grammars and corpora
===============================
Scripts for generating artificial grammars and corpora ("fake
languages"). Configuration files are in [../0-config](../0-config).

Edit [../0-config/0-pipeline.sh](../0-config/0-pipeline.sh) and
[../0-config/1-dict-conf.scm](../0-config/1-dict-conf.scm) and
[../0-config/1-corpus-conf.sh](../0-config/1-corpus-conf.sh) to
configure.

Then run `gen-dict.sh` to create a dictionary, and run `gen-corpus.sh`
to create a corpus. That's it. Now move to step
[../2-word-pairs](../2-word-pairs) next.

File overview
-------------
* `gen-dict.sh` -- This generates a random dictionary encoding an
   artificial grammar.  The structure of the dictionary is configured
   in `../0-config/1-dict-conf.scm`.

* `gen-corpus.sh` -- This generates a random corpus from the fake
   grammar. The location of the dictionary, and the corpus, is
   configured in `../0-config/1-corpus-conf.scm`.

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

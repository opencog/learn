
Artificial grammars and corpora
===============================
Scripts for generating artificial grammars and corpora ("fake
languages"). Configuration files are in [../0-config](../0-config).

Edit [../0-config/0-pipeline.sh](../0-config/0-pipeline.sh) and
[../0-config/1-dict-conf.scm](../0-config/1-dict-conf.scm) and
[../0-config/1-corpus-conf.sh](../0-config/1-corpus-conf.sh) to
configure.

Then run `gen-corpus.sh`. That's it. Now move to step
[../2-word-pairs](../2-word-pairs) next.

File overview
-------------
* `gen-dict.scm` -- Generates a random artificial ("fake") grammar.
  Requires configuration paramters to be declared; see
  `../0.config/dict-conf.scm` for an example configuration file.

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

* `gen-corpus.sh` -- This generates a random corpus from the fake
   grammar. Edit and configure as desired. This automatically invokes
   `gen-dict.scm` to create the dictionary. The location of the
   dictionary, and the corpus, is configured in `dict-conf.scm`.

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

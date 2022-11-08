
Artificial grammars and corpora
===============================

Configuration files for various parts of the processing pipeline.

File overview
-------------
Files are numbered so that they appear in the same order (roughly) as
they are needed to run the processing.

* `0-pipeline.sh` -- Master configuration file, contains parameters
  that are generic to the entire pipeline. Set the environment variable
  $MASTER_CONFIG_FILE to the location of this file.

* `1-dict-conf.scm` -- This contains the configurable parameters for
   generating a random artificial ("fake") grammar. Edit this file
   as desired (or copy it and edit it...). Once the parameters are
   configured as desired, the grammar can be generated as
```
   $ /run/1-gen-dict/gen-dict.scm 1-dict-conf.scm
```
   Skip this, if not working with artificial languages.

* `1-corpus-conf.sh` -- This contains parameters that control the size
   of the generated corpus.

* `2-pair-conf.sh` -- Parameters that control (word-)pair counting.
  Pre-configured variants for English and artificial languages are in
  `2-pair-conf-en.sh` and in `2-pair-conf-fake.sh`. A variant for
  exploring disassembled binaries is in `2-pair-conf-objdump.sh`

* `3-mst-conf.sh` and `3-mpg-conf.sh` -- Parameters that control MST
  parsing and disjunct counting. The MPG variant is the "Maximum Planar
  Graph" variation of MST, as ordinary MST parsing may result in
  non-planar trees.

* `4-gram-conf.sh` -- Grammatical clustering.

Notes
-----
What is the best way to automate complex data processing pipelines?
I don't know. For now, an ad hoc collection of shell scripts is used.
Perhaps one of the following systems would be better?

* [Common Workflow Language](https://www.commonwl.org/user_guide/index.html)
  `apt install cwltool` -- the documentation reveals that it is very
  verbose. It also does not seem to offer a way of storing config
  parameters...

* Something from the [Awesome Pipeline](https://github.com/pditommaso/awesome-pipeline)
  list.

The above all seem far too complicated. This project needs something
simple, basic, easy-to-set-up, easy-to-undertand, easy-to-modify. So,
for now, stick to plain-old bash scripts.

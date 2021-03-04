
Artificial grammars and corpora
===============================

Configuration files for various parts of the processing pipeline.

File overview
-------------
Files are numbered so that they appear in the same order (roughly) as
they are needed to run the processing.

* `0-pipeline.sh` -- Master configuration file, contains parameters
  that are generic to the entire pipeline.

* `1-dict-conf.scm` -- This contains the configurable parameters for
   generating a random artificial ("fake") grammar. Edit this file
   as desired (or copy it and edit it...). Once the parameters are
   configured as desired, the grammar can be generated as
```
   $ ../1-gen-dict/gen-dict.scm 1-dict-conf.scm
```

* `1-corpus-conf.sh` -- This contains paramaters that control the size
   of the generated corpus.

* `2-pair-conf.sh` -- Parameters that control word-pair counting.
  Pre-configured variants for English and artificial langauges are in
  `2-pair-conf-en.sh` and in `2-pair-conf-fake.sh`.

* `3-mst-conf.sh` and `3-mpg-conf.sh` -- Parameters that control MST
  parsing and disjunct counting. The MPG variant is the "Maximum Planar
  Graph" variation of MST, as ordinary MST parsing may result in
  non-planar trees.

Notes
-----
What is the best way to automate complex data processing pipelines?

* [Common Workflow Language](https://www.commonwl.org/user_guide/index.html)
  `apt install cwltool` -- the documentation reveals that it is very
  verbose. It also does not seem to offer a way of storing config
  parameters...

* Something from the [Awesome Pipeline](https://github.com/pditommaso/awesome-pipeline)
  list.

The above all seem far too complicated. We're sticking to plain-old
bash scripts for now.

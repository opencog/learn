
Configuration Files
====================

Configuration files for various parts of the processing pipeline.

File overview
-------------
Files are numbered so that they appear in the same order (roughly) as
they are needed to run the processing.

* `0-pipeline.sh` -- Master configuration file, contains parameters
  that are generic to the entire pipeline. Set the environment variable
  $MASTER_CONFIG_FILE to the location of this file.

* `2-pair-conf.sh` -- Parameters that control (word-)pair counting.

The following two directories are copied (by `make install`) to
`/usr/local/share/link-grammar`, so that Link Grammar can find them.

* `dict-pair` -- Directory containing a Link Grammar dictionary
  that allows Link Grammar to use word-pair data held in the AtomSpace.
  The resulting parses will be Maximum Planar Graph (MPG) parses.

* `dict-combined` -- Directory containing a Link Grammar dictionary
  that allows Link Grammar to use grammatical info held in the
  AtomSpace. This includes both word-pair MI data, as well as disjunct
  data. If the AtomSpace contains only word-pair data, then the resulting
  parses will be Maximum Planar Graph (MPG) parses. If it also contains
  disjunct data, then those will be used preferentially, with word-pair
  data making up for missing disjuncts.

* `3-mpg-conf.sh` -- Parameters that control MST/MPG parsing and
  disjunct counting.

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

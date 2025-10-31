
Osolete Parse management scripts
========================

The scripts here are obsolete; they were previously used to automate
the operation of the language-learning pipeline.


* `fake-lang` -- Link Grammar boilerplate, required for working with
   a Link-Grammar file-based dictionary. The `gen-dict.scm` file copies
   these files to the target dictionary location. For example:
```
   $ cp -r fake-lang /home/ubuntu/data/trial-run/fake-lang
```
* `gen-dict.scm` -- Generates a random artificial ("fake") grammar.
  Requires configuration parameters to be declared; see
  `../0-config/dict-conf.scm` for an example configuration file.

   Once the parameters are configured as desired, the grammar can
   be generated as
```
   $ ./gen-dict.scm dict-conf.scm
```

* `submit-one-netcat.pl` -- Uses `netcat` to submit files for processing.
  This tripped over an operating system infelicity; replaced by a version
  that uses TCPIIP sockets directly.

* `split-objdump.pl` -- Work with binaries. Contract work done for
  Object Security LLC. The bastards never paid me for this work.
  These guys: https://objectsecurity.com -- they're assholes.
  They owe me money, they ghosted me after several months of real work.
  Scumbags.

* `relex-server-any.sh` -- Uses the relex server to perform the ANY parse.
  RelEx is obsolete.

* `fake-lang` -- Link Grammar boilerplate, required for working with
   a Link-Grammar file-based dictionary. The `gen-dict.scm` file copies
   these files to the target dictionary location. For example:
```
   $ cp -r fake-lang /home/ubuntu/data/trial-run/fake-lang
```

* `export-dictionary.scm`: Export the result of classification as
  a Link Grammar-compatible dictionary. Export format is as an
  SQLite3-format LG dict.  No longer used; LG can worked directly from
  the atomspace.

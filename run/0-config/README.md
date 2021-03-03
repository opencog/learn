
Artificial grammars and corpora
===============================

Edit `gen-dict.scm` and adjust parameters as desired.

Edit `gen-corpus.sh` and set directory path to where you want the
generated corpus to go. Then run it.

File overview
-------------
* `dict-conf.scm` -- This contains the configurable parameters for
   generating a random artificial ("fake") grammar. Edit this file
   as desired (or copy it and edit it...). Once the parameters are
   configured as desired, the grammar can be generated as
```
   $ ../1-gen-dict/gen-dict.scm dict-conf.scm
```

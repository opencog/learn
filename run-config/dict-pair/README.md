MPG Parsing Atomese dictionary
------------------------------
This is an example configuration for an AtomSpace-backed dictionary,
configured for parsing using word-pair MI data. The resulting parses
are Maximum Planar Graph parses: parses that draw links between words
in such a way as to maximize the MI between word-pairs.

For general info about Link Grammar dictionaries running out of the
AtomSpace, consult the Link Grammar documentation.

* The `4.0.affix` file specifies punctuation that will be automatically
  split off of words.

* The `storage.dict` file specifies assorted configuration data,
  including the location of MI data, and how to scale it to obtain
  LG-style "costs".


Parse management scripts
========================

The scripts here are used to automate the ingestion of plain-text
UTF-8 files into the language learning pipeline.  These can be applied
to any flat text files from any origin of your choice.  Some tools
for downloading Wikipedia and Project Gutenberg texts can be found
in the `../download` directory.

You will typically want to make copies of these, and tailor them to
your specific needs and procedures. In particular, many of these
files require database credentials to be set; the exact credentials
to use will depend on which copy of which database you are using.
You WILL be copying around a lot of databases!

A quick overview:

* `submit-one.pl`: script to send sentences to the cogserver.

* `split-sentences.pl`: split text into sentences. Accepts free-form text,
  and looks for likely end-of sentence locations, so that there is one
  sentence per line.


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

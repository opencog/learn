#! /bin/bash
#
# Configuration parameters for word-pair counting.
#
#
# Enable or disable sentence splitting.
# If the text corpora have one sentence per line, then splitting is not
# needed. If the corpora are arranged into paragraphs (as conventional
# for natural language), then the paragraphs must be split into distinct
# sentences.
SENTENCE_SPLIT=false

# If splitting is enabled, then specify the splitting language. Choices
# include `en`, `fr`, `pl` and many more; see the splitter directory for more.
SPLIT_LANG=en

# IPv4 hostname and port number of where the cogserver is running.
HOSTNAME=localhost
PORT=17008


Extracting Meaning
==================
This text sketches an algorithm for extracting meaning from natural
language text. The intended sense of "meaning" here is the conventional
one: what you might find in a dictionary or encyclopaedia, albeit
in machine-readable format. A more precise definition will become clear
as the algorithm is sketched out.

It seems to be time to start this: there's been fair success with
learning grammar, and that code seems to be debugged enough and generic
enough that it should be able to support the next level, namely, this
project.

The rough idea is to apply the same algorithm as before: of finding
pairs, and then creating disjuncts, except this time, the pairs will be
pairs of Sections, instead of Words. I believe that such pairs of
Sections will be able to capture idioms, instititional phrases, and the
like with ease. I believe the corresponding MetaSections will be able to
capture conceptual information, such as "intentional" properties of
objects (e.g. animals are furry, have a tail, have for feet, can run,
have babies, and need to eat.) I believe MetaSections will also be
capable of performing anaphora resolution.

Details
-------
Since this is "same algorithm as before", belaboring it seems pointless.
Thus, we need to pin down the differences. As below.

### Corpus Selection

The pair-window needs to be much larger, spanning multiple sentences.
This means that the corpus must consist of single "coherent" texts,
such as novels or other kinds of long books. Short texts would be bad,
unless they are all on the same topics. Ideal might be 5 or 10 different
school texbooks on exactly the same topic.

### Windowing

Anaphora resolution requires establishing long-distance relationships
between words in distinct sentences. Thus, the window-size for
pair-counting needs to be two or three sentences wide. Maybe three
sentences backwards, and one sentence forwards. But how to count
sentences? We don't want to make ad-hoc assumptions about punctuation.
So, for natural language, assuming a window width of about 30 to 70
words seems about right.

### Pair counting

The first step is to just count pairs within this window. There will
be vast numbrs of these: for width N there are N^2 pairs.

The pairs that need to be counted need to be the actual pseudo-csets
used in the parse, and not the word-class disjuncts.  The word-classes
give us the parse, but the explicit words are the things carrying
meaning, not the syntactical classes used for parsing.

Should only the top-ranked parse be counted?  Maybe all parses within
some cost-window of the top-ranked parse? Maybe the top N parses? The
problem here is with weighting. If we use only the top-ranked parse, and
its crummy, then the crummy disjuncts get counted.  If we use a
cost-window, then all the places where the parses agree will be
recounted, giving them a strong weight (good!) and all the places where
they disagree get a low weight (also good.)  The problem here is that
if one sentence has 10 parses in the cost-window, and another has only
two, then the first sentences will outweigh the second by 10/2 = 5 and
clearly that's wrong. But if we fix N, say, at 8, then the second
sentence will still have 2 good parses, and 6=8-2 crummy ones that get
counted. So apparently the correct answer is to use a cost-window, but
then rescale counts by the number of parses found.

A different issue is that different sentences might have a different
number of parses, even though they are within the same window. This now
gets us into the uncharted territory of parsing without distinct
sentence boundaries. Full stop. It seems this needs to be adequately
resolved first.

### Sentence Boundaries

See immediately above. It's unclear how the window interacts with
sentence boundaries, and with how parsing interacts with that. This will
need to be explored and resolved.

Gut feel say that we won't need to make ad hoc assumptions about
puncutation. For two reasons: First, the earlier stages did use an
explicit `###LEFT-WALL###` and that has been learned, and is currently
hard-coded into the grammar. In retrospect, this may be a design flaw:
the only way that the wall can be inserted is if the earlier stages had
some ad hoc sentence detection in them. So it seems that this needs to
be fixed.  Ouch. More work.

Anyway, lets pretend there are no `###LEFT-WALL###` markers in the
dicts.  I suspect that sentence boundaries can be detected "naturally",
by having breaks during parsing, i.e. by having no disjuncts, or only
very expensive disjuncts in the sentence-gaps. This will need to be
explored.

That is, during parsing, perhaps one could maintain a "running cost" of
the disjuncts used in a narrow window, say, 3 or 5 words wide. This
running ost presumably bounces up and down, and whenever it becomes
huge, then that indicates a sentence boundary.

The issue is that the current LG parser does not have this running-cost
window mechanism.  Its not obvious how to add it.

Flip side: we don't really need sentence boundaries, anyway, to perform
the pair-counting. The only reason we're getting into this mess is
because the LG parser prefers to work with sentences, and its not clear
what will happen if we just feed it 30 contiguous words...

Perhaps we need an explicit ellipses marker, to indicate the beginning
and end of the window to the parser. The explicit ellipsis will act as a
source or sink for all unconnected disjuncts near the window edge.

With ellipsis markers, we can then add a guard window, say 3 or 5 words
before/after the ellipsis, where pairs won't be counted, because the
chosen disjuncts are nasty because they're disturbed by the ellipses.
This seems like the best idea!  Except we need to ... code this up.

### Trimming
Gut instinct says that the above will generate vast numbers of pairs,
clogging up RAM. Thus, its likely that trimming will have to be deployed
early and often. Controlling this will need experimentation.

### MetaSections

### Mihalcea algo!

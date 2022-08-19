# Automated Grammar Induction
## Experimental Results

Hello.

I am going to give this talk in two parts.  The first part is identical
to the lightning talk I will deliver on Monday; it provides a neccesary
backgrounnd to the longer and drier results I wish to present today.

------------
## Slide 1: Word-pair Mutual Information

OK I need to start with some basic definitions.

A word pair is just an ordered pair of two words.

The number of times a pair has been observed is denoted by N.

This allows a frequentist probability to be assigned.  Note that
the total probability sums to 1.0

The star denotes a wild-card sum over all entries in that location.

This can be used to define the mutual information betseen a pair of
words. For the moment, I will call it the "Lexical Attraction"
because it is not left-right symmetric.

Conventional textbooks only present a symmetric version of MI that is
inappropriate for ordered pairs.  This can be a point of confusion.

I have a strong information-theoretic bias.

This is because there are very powerful reasons to beleive that counting
bits, counting information is the correct way to understand ...
information.

I remark on this because some people really do not like it! They suggest
other metrics, measures, other approaches, possilities.

I say bunkum.  It just means you don't have the correct theoretical
framework in place to understand what is actually happening.
You didn't have a way of measuring the information content of the
transaction.

You didn't have a model of the interaction.

------------

## Slide 2: Characterizing Word-Pair Data Sets

The study of pairs is the study of matrices.  The word-pair dataset
is a matrix; rows and columns are left and right words in the pair.
It is a sparse matrix; most word-pairs are not observed.

Matrices has some global properties, a collection of numbers
characterize the general shape.

I have the log width and height.  Everything is a log because that
puts everything on the same scale.  Makes everything comparable.
We measure in bits, so the log is log base two.

Left and right are not equal for mundane reasons:
The word "the" doesn't appear at the ends of sentences.
Punctuation does not appear at the start of sentences.

Interesting numbers include the total number of non-zero entries, and
the total number of observations. Expressing these in a relatvie
fashion, relative to the size of the matrix, provides better intuition.

Thus, the sparsity, which is the fraction of non-zero entries,
and the rarity, which experimentally appears to be independent of the
dataset size.

Entropy is not that interesting, but it is conventional to report it.
If I'm going to be "information-theoretic" and stay true to my beleifs,
this is obligatory to report.

One also has a marginal entropy, for the left and the right (the rows
and columns) It is written in the "margins" which is why it's called
"marginal".

There is also the total MI.

------------

## Slide 3: Example Word-Pair Data Sets

Lets take quick look at some datasets. I understand this slide can
induce audience pain, so I will try to be quick. Sorry in advance.

The smallest dataset is on the left: 100K x 100K words
vs. 500K x 500K on the right.

This is NOT a "large" language model. This fits on conventional
desktops. It is approx 1 million times smaller than the Google LLM's.
Six orders of magnitude. Let that sink in.

If you scane D-total line, you'll see up to 50 million non-zero entries.

The sparsity, the fraction varies from 1 in a thousand to 4 in a
thousand non-empty pairs.  This is very sparse. This is a driver of
software development: You MUST have a way of representing sparse
data in your system. Pretty much all conventional math/probability
science frameworks don't suport this; this is a roadblock.

The rarity, defined earlier, appears to be independent of data-set size.
I think this is meaningful, but don't yet know what it means.

The number of observations per entry grows, the more observations you
make. This is expected.

The total entrop grows slightly.

The total mutual information stays constant or maybe even drops, as the
dataset size increases.

It's not very large. There is not a lot of data in word-pairs.

Think of the MI as a measure of the amplification of information.
Given some data set, it tells you how much information you were able
to collect up and concentrate.

One and a half bits of information is perhaps laughably tiny.
But it is positive, and greater than zero.


------------

## Slide 4: Sample Size Effects

A short detour on sample size effects.

The vertex degree is a classical statistic from the early days of the study
of large graphs, which tended to be scale-free graphs with preferential
attachement.

I don't think its useful in the present case, but reviewers clamor for
this stuff because all the classical papers talk about it.

You can see the graph is Zipfian.

I have no intuition or theoretical explanation for why the exponent,
the slope, is -1.6

One the left side, at te top of the chart, are "words" that occur
in only a handful of pairs.

Dataset inspection reveals that it is junk.
Bad punctuation, typos, bad quote segmentation, stray HTML markup.

Basically, if a typo occurs only once, then yes, it will be in very
very few word-pairs, which translates to having a vertex with a very
low degree. Degree is just the number of other vertexes connected to it.

Amazingly, almost 2/3rds of the dataset consists of this kind of junk!

It eats up RAM! Storage! Yow!  Data set trimming is not a bad idea.

By comparison, the word "the", and punctuation, occur on the right
right-hand side of this graph.  The word "the", and punctuation, occur
in a vast number of word pairs, and so have a very large degree.

------------

## Slide 5: MI Distribution

The distribution of the mutual information is far more interesting.
That is shown here.

28 Million word pairs were observed. Agin, this is microscopic, compared
to present-day large-language models.

The distribution breaks down perfectly into the sum of two curves, and I
do ean perfectly, its actually quite amazing.

One is a gaussian, centered roughly at zero. The other is a log-normal
curve.

I've never seen this sort of data discussed before, so I don't know of
any theory for it. However, I can offer a simple explanation.

The gaussian centered at zero is entirely due to random sampling
effects.  it is what you would get if you had a bag of words, and
randomly sampled two (with replacement) out of that bag. Classic
analytic combinatorics.  The result is a Gaussian centered at MI=0.

The rest, the log-normal curve, is presumably "meaningful English
Langauge correlation".

But how do you tell which is which?

Well, above an MI of 10, it really is "English".

Between 2 and 10, its ... what? Maybe random junk? Maybe English?

By the way, if you repeat this for Madarin Chinese, you get a
more-or-less identical figure.

------------

## Slide 6: MST parsing

So ... how to proceed. The next step was sketched out 25 years ago,
Maximum Spanning Tree Parsing.  Here, you take a sentence, consider all
possible spanning trees on that sentence, and then pick the one with the
highest total MI.

The new thing, the innovation I am focused on is the jigsaws.

Cut each edge in half, and label the endpoints. This results in a word
with some connectors on it.  This is the jigsaw.

Elsewhere, these get called disjunts, because they are "disjoined" from
other occurances. I'm still fishing for a good vocabulary for this
stuff. My favorite today is "jigsaws".

Anyway, count these. Do an MST parse of a large number of sentences, and
count how often each jigsaw is observed.

This gives you another set of pairs, another matrix.  The columns of the
matrix are words, the rows are connector-sequences.

The matrix is rectangular, there are about 10 times more connector
sequences than there are words.

------------

## Slide 7: Disjunct Dataset

I'm not going to talk about this slide. I'm going to skip it.

It shows what happens when you trim away infrequent pairs.
So, for example, trim away words that are seen only twice,
or less than 5 or 10 times.  Trim away matrix entries that
are seen only once of twice.

This strongly shrinks the dataset size.

This trims away junk, typos in the dataset.But you have to be careful,
as some of teh low-frequency stuff can be meainingful:

They can be
* Geographical place names, used only a few times.
* Product names, given names used only a few times.
* Foreign-language loan words used only a few times.

Trimming is dangerous.

It reduces dataset size, but looking at the last row, timming lowers the
overall MI gain.

Perhaps the main thing to mote here is that the total MI is much much
larger than that for word pairs. An MI of 9 or even 6 is much larger
than the 1.5 seen for word pairs. We've acheived significant
amplification gain.

In radio electronics, the very first transistor after the antenna has a
gain of about 2 to 3, this is comparable to the MI for word-pairs.
The nex stage of a radio amplified can have a gain of 100 or 1000.
That is what we are seeing here.  There seems to be a real information
amplification gain.

------------

## Slide 8: Disjunct MI

Here, I've raps the MI for the jigsaws, just as before.

I don't see what what's going on here. The meaning is opaque. it's a
bit of a mess. The various curves are for the different trim levels.

Gets closder to being a Gaussian, the more strongly trimmed it is.

But not close enough to be "satsifying" in any way.

------------

## Slide 9: Distribution of Similarity

Lets try again for the similarity betwen words, with similarity being
computed from the dot product of vectors.

Wow! What a world of difference! A nearly perfect Bell curve!
It's stunning!

------------

## Slide 10: Similarity Metrics

OK, what did I just show?


VI just shifts the curve over to the right. Curve does not change.

Cosine distance is terrible (worst possible)

Why? Cosine distance only makes sense on Euclidean spaces.
The space of probability is not Euclidean!  Its a Simplex!

------------

## Slide 11: Spin Glasses

------------

## Slide 12: Similarity and Clustering

------------

## Slide 13: Word-sense Disambiguation

------------

## Slide 14: Conclusion

------------

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

Vertex degree is a classical statistic from the early days of the study
of large graphs, scale-free graphs and preferential attachement.

I don't think its useful, but reviewers clamor for this stuff.

It's Zipfian.

I have no intuition or theoretical explanation for why the exponent is -1.6

One the left side are "words" that occur in only a handful of pairs.
Dataset inspection reveals that it is junk.
Bad punctuation, typos, bad quote segmentation, stray markup.

If a typo occurs only once, then yes, it will be in very very few
word-pairs.

Amazingly, almost 2/3rds of the dataset consists of this kind of junk!

It eats up RAM! Stoarage! Yow!

------------

## Slide 5: MI Distribution

That is, consider a vocabulary of 100K words, and then randomly,
uniformly sample 1M word-pairs.  This strongly under-samples all
possible word pairs. The result would be a Gaussian centered at MI=0.

The rest is presumably "meaningful English Langauge correlation",
but how do you tell which is which?

Well, above an MI of 10, it really is "English".

Between 2 and 10, its ... random junk? English?

------------

## Slide 6: MST parsing

------------

## Slide 7: Disjunct Dataset

Skip this slide.

Notable: trim away pairs that are seen only once or twice,
Trim away words that are seen 2 or 5 or 10 or fewer times.

Strongly shrinks the dataset size.

Beside typos, these can also be meaningful:
* Geographical place names, used only a few times.
* Product names, given names used only a few times.
* Foreign-language loan words used only a few times.

Trimming is dangerous.

------------

## Slide 8: Disjunct MI

Opaque.
Gets closder to being a Gaussian, the more strong trimmed it is.

But not close enough to be "satsifying" in any way.

------------

## Slide 9: Distribution of Similarity

Wow!

------------

## Slide 10: Similarity Metrics

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

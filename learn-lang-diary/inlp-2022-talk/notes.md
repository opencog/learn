# Automated Grammar Induction
## Experimental Results

# Slide 1: Word-pair Mutual Information

Here are some basic definitions.

Note the total probability sums to 1.0

It's called "Lexical attraction" because it is not left-right symmetric.

Conventional textbooks only present a symmetric version of MI that is
inappropriate for ordered pairs.  This can be a point of confusion.

I will take a strong information-theoretic bias.

This is because there are very powerful reasons to beleive that counting
bits, counting information is the correct way to understand ...
information.

I remark on this because some people really do not like it! They suggest
other metrics, measures, approaches, possilities.

I say bunkum.  Just means you don't have the correct theoretical
framework in place to understand what is actually happening.
You didn't have a way of measuring the information content of the
transaction. you didn't have a model of the interaction.

------------

# Slide 2: Characterizing Word-Pair Data Sets

Boring but important.

If you can't follow this, you'll be lost for the rest of the talk.
I'm not here to present SciFi Hollywood movie plot synopsis.

The word-pair dataset is a matrix; rows and columns are left and right
words.

A collection of numbers characterize the gross properties of that
matrix.

Everything is a log because that puts everything on the same scale.
Makes everything comparable.

Left and right are not equal for mundane reasons:
The word "the" doesn't appear and the ends of sentences.
Punctuation does not appear at the start of sentences.

Entropy is not that interesting, but it is conventional to report it.
If I'm going to be "information-theoretic" and stay true to my words,
this is obligatory to report.

------------

# Slide 3: Example Word-Pair Data Sets

Smallest on the left: 100K x 100K  vs. 500K x 500K

This is NOT a "large" language model. This fits on conventional
desktops.

I'm going to waste some time reviewing the rows and columns here.

------------

# Slide 4: Sample Size Effects

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

# Slide 5: MI Distribution

That is, consider a vocabulary of 100K words, and then randomly,
uniformly sample 1M word-pairs.  This strongly under-samples all
possible word pairs. The result would be a Gaussian centered at MI=0.

The rest is presumably "meaningful English Langauge correlation",
but how do you tell which is which?

Well, above an MI of 10, it really is "English".

Between 2 and 10, its ... random junk? English?

------------

# Slide 6: MST parsing

------------

# Slide 7: Disjunct Dataset

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

# Slide 8: Disjunct MI

Opaque.
Gets closder to being a Gaussian, the more strong trimmed it is.

But not close enough to be "satsifying" in any way.

------------

# Slide 9: Distribution of Similarity

Wow!

------------

# Slide 10: Similarity Metrics

VI just shifts the curve over to the right. Curve does not change.

Cosine distance is terrible (worst possible)

Why? Cosine distance only makes sense on Euclidean spaces.
The space of probability is not Euclidean!  Its a Simplex!

------------

# Slide 11: Spin Glasses

------------

# Slide 12: Similarity and Clustering

------------

# Slide 13: Word-sense Disambiguation

------------

# Slide 14: Conclusion

------------

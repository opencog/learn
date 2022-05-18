
A Unified Architectural Framework
=================================

There are many -- many dozens -- of different NLP projects and
frameworks -- written in Python, Java, Perl, and another half-dozen
programming ecosystems.  The code here uses none of these. Why?

The answer is illustrated by a a simple, practical problem encountered
when one tries to perform scientific research withing these frameworks.
It is illustrated below, an excerpt from an email.

A Simple Unsupervised Learning Example
--------------------------------------
One dream that I have is to be able to have feedback between processing
stages. So, for example: tokenization delivers 3 or 4 different
tokenization possibilities, each ranked with a score (ideally, log_2 of
probability or similar) All of these possibilities are then run through
the parser, which also assigns scores (again, log_2 p).  The sum of
these scores can then be used to select that most likely
tokenization+parse. (A sum -- addition, because logarithms are
additive). Thus, the parser helps eliminate bad tokenizations.

That would be the first step. The second step is more interesting:
Given the above, one can then automatically search for errors in the
tokenization. That is, to automatically discover rules that will correct
errors. The final system is then "use the freedom model algorithm,
unless rule A applies or unless rule B applies ..."  where rule A, B, ...
were obtained by statistical analysis of tokenizer plus parser.

This exposes the problem: to do the above, one needs to perform a
statistical analysis of the tokenizer+parser, working together. What
code performs this statistical analysis? Where is the statistical data
kept? What converts this analysis into rules? What is the format of the
rules? How are the rules applied? What code applies those rules?

If one is naive, then the answer to all of those questions is difficult,
ugly, messy, confusing, and takes a lot of work. How can we make this
less messy, less difficult? (Less stovepipe code?) My answer, as always,
is to say that all work must be done in a single, common framework, that
makes it easy to collect statistical information from a large variety of
sources. A place where the statistical analysis is easy. A place where
inference can be automated. A place where rules can be defineed,
generically, and lanuched, generically.  That single location is, for me,
the AtomSpace ...

I'm saying this to explain why, if it wasn't clear before, why I do
things the way I do them.

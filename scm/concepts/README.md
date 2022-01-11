
Concept Discovery
=================
This directory contains experimental code for the automated discovery
of conepts. Details won't become apparent until the experiments are done.

The general idea is that, given a continguous text corpus, some words
will recurr repeatedly in that corpus. A starting assumption is that
such recurring words refer to "the same thing". The properties or
attributes of that thing are revealed by parses of the sentences: the
parses establish connections between the "things" and thier "properties".

The general issue is that, at this stage, we do not yet know what nouns,
verbs, adjectives, modifiers or references are. Some of these "same
things" will be nouns: common nouns or specific entities. Some will be
action verbs (actions).  Some of these "same things" will be referrences
to other things. The hope is that these can be discerned using principles
of mutual information (again).

The previous paragraph talks about nouns and verbs, although it would be
more accurate to talk about concepts: the goal is to discover concepts.
Perhaps, as a by-product, we'll be able to discern action-concepts (verbs)
from existance-concepts (nouns), as well as resolving references.

The starting point is a collection of parses from a text. The links
connecting the parses are to be traced out two or three or more steps
away, and similarities are to be computed for this local neighborhood.
Perhaps this can be done by leveraging earlier code and algos? But
there's a big difference: the earlier work had a very short coherence
scale: just one sentence, and no more. By contrast, the coherence length
here is meant to be several sentences or a paragraph in size, and common
properties are collected over the entire text (book chapter or article.)
It is fundamentally important to NOT mix up concepts learned from one
text with those learned from another text. Figuring out of two distinct
texts are talking about the "same thing" will have to be done at a
different stage.

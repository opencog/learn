

# Learning Topology and Geometry

Hello.

I've come today to talk about a symbolic approach to AGI.

This talk will be a two parter, the first part will be the lightning
talk I will be delivering on Monday; I'm practicing it today, because
the main presentation won't make sense without it.

---------------

## Slide 1: A Lack of Topological and Geometric Awareness.

Topology and stucture of 3D objects is a challenge for DL/NN.
I found these on twitter recently:

* Someone asked DALL-E for a "coffee cup with hole" and got this image.
* Another unspecified AI measured a 5.2-meter-long cow.

I can only conclude: AI can "see", it just doesn't "understand"
what it sees.

This talk proposes the next step on the way to "understanding":
a combination of tokenization and grammar.

---------------

## Slide 2: Conventional Simplical, Cellular Homology

We already have a toolset for discussing topology and geometry.

Its provided by conventional mathematical and engineering thinking.

The images show triangular meshes: vertex-edge lists AKA "graphs".

This domain already offers a broad and deep menu of concepts to apply.

---------------

## Slide 3: Reframe: Edge Lists -> Jigsaws with Connectors

The twist proposed here is to replace the conventional edge-list
description of a graph by a jigsaw-puzzle-piece description.

Jigsaw puzzle pieces can mate only when the connectors can mate.

The mating types are type-theory types.

The result is an unconventional graph theory, and unconventional
algebra.  It's well known and commonplace in linguistics.

But first, another example.

---------------

## Slide 4: Connectors Indicate Symbolic Relationships

Here, I've manually segmented an image into some "obvious"
relationships:

* Certain colors that must occur above or below certain other colors
* Certain shapes are shared by all the colored regions.
* There is a background setting for the object of interest.

The partially-assembled jigsaw indicates the object. The subassemblies
indicate the part-whole relationships.

I dwell on part-whole, since there are papers in the DL/NN world,
written by leading figures, that lament the lack of part-whole
relationships in DL/NN.  (Geoff Hinton?)

---------------

## Slide 5: Jigsaw Paradigm Established in Linguistics

This idea of assembly from parts is not new; it appears explicitly in a
figure in a paper on linguistics from 1991, the original Link Grammar
paper, and implcitly in earlier work (once you know what to look for).

The paradigm has been repeatedly rediscovered by linguists.

One of them, (EA Nida) spoke of assembling words into sentences
the way that chemists assemble atoms into molecules.

Bottom of the slide shows how to bridge from this approach to a
statistical approach.  It shows a maximum spanning tree parse,
with the weights indicated in the links.

The bottom image is from a PhD thesis from 1998.

---------------

## Slide 6:  Provides Semantics for Symbolic AI

Jigsaw pieces provide semantics for symbolic AI.

This slide applies the jigsaw connector paradigm to find things
that can be seen -- on the right, you see jigsaw pieces for a telescope,
a lens and for eyes; they have the same shape because telescopes,
lenses and eyes all have similar relationships to the perceived object.

Lingusits have long observed that "pure syntax" already encodes the
shallower layers of semantics.  You can get the "meaning" of words
simply be seeing how they are "connected" to ther surroundings.

---------------

## Slide 7: Not Just 1D, 2D, 3D, but also Abstract Sensory Domains

This idea works not just for language and vision but also
for general sensory domains.

On the left is a clip of a whale song recorded by NOAA.

It has a syntactic structure that can be represened via jigsaw
conectors.

Note that there are at least five dimensions here:
frequency, intensity, time, envelope, chirp modulation.
More generally, wavelet-style decomposition is handy for
high-dimensional fractal spaces.

I've oversimplified the segmentation to make it fit in this chart.
The true structure will be more complex.

---------------

## Slide 8: Segmentation and Tokenization as (evolutionary, ML) Program Learning

This slide attempts to illustrate some random audio filter sequences
used to detect features.  The task is to discover and data-mine good
filter sequences (as opposed to having an engineer design one).

Standard ML techniques can be applied to acheive this.

The point here is that this jigsaw paradigm is not pie-in-the-sky
theory.  There are practical algorithms that can extract this structure.

They are old-school. Conventional machine learning.

Can DL/NN be adpated to this?

Maybe. I've got some wild ideas on how to do this.  No one has
explored the topic.

---------------

## Slide 9: Experimental results

I've personally walked part-way down this path, creating software to do
all this. It's slow-going.  It's experimental research with some
theoretical development behind it.

Perhaps the most remarkable result is this Gaussian.

My apolgies, if you were hoping for a LaMDA-like large language model.

This Guassian, a "Bell Curve", shows the distribution of the MI
between pairs of jigsaw pieces. Recall, tehse jigsaw pieces encode the
grammatical relationships, they encode the syntax of the language.

It's remarkable because Gaussians indicate that the underlying data is
uniformly distributed on the surface of a high-dimensional
(projective) sphere.

In physics, this is called a "spin glass".  The study of spin glasses
is brand new; its a very young branch of mathematics.

It studies probability in high dimensions.

It's nothing at all like the low-dimensional probability theory of
college textbooks. It really is a brand-new world and it shows in
the publications.

I find it remarkable that this is a kind of experimental demonstration
that the syntax of the English language is maximially efficient in it's use of
grammatical relationships.

---------------

## Slide 10: Common Sense as Inference over Symbolic Domains

I'd like to conclude with a conjecture, a bit of science-fiction,
as it were.

The roadway I've sketched appears to be recursive. The tricks can be
applied repeatedly, to mine deeper into the semantic direction.

Common sense can be learned. It's just a set of frequenctly-observed
relationships.

This is a panpsychic view of semantics: if everything is a graph,
if semantics arises (purely) from relationships, then "all that we have
to do" is to mine the relationships ever more deeply.

The jigsaw paradigm provides a practical tool, both theoretically and
software-wise, in which this can be performed.

Thank you.

---------------

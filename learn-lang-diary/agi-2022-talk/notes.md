

#Learning Topology and Geometry


# Slide 1: A Lack of Topological and Geometric Awareness.

Topology and stucture of 3D objects is a challenge for DL/NN
Found on twitter recently:

* Someone asked for DALL-E for a "coffee cup with hole" and got this.
* Some unspecified AI measured a 5.2-meter-long cow.

Conclude: AI can "see", it just doesn't "understand" what it sees.

This talk proposes a step on the way to "understanding": tokenization +
grammar.

---------------

# Slide 2: Conventional Simplical, Cellular Homology
Mathematicians and engineers already have a toolset for discussing
topology and geometry.

It's broad and deep.

The images show triangular meshes: vertex-edge lists AKA "graphs"

---------------

# Slide 3: Reframe: Edge Lists -> Jigsaws with Connectors

The twist proposed here is to replace the conventional edge-list
description of a graph by a jigsaw-puzzle-piece description.

Jigsaw puzzle pieces can make only when the connectors can mate.

The mating types are type-theory types.

The result is an unconventional graph theory, and unconventional
algebra.

But it's well known, commonplace in linguistics. First, another example.

---------------

# Slide 4: Connectors Indicate Symbolic Relationships

Here, I've manually segmented an image into some "obvious"
relationships:

* Certain coilors that must occur above or below certain other colors
* Certain shapes shared by all the color regions.
* A background setting for the object of interest.

The partially-assembled jigsaw indicates the object. The subassemblies
indicate the part-whole relationships.

I dwell on part-whole, since there are papers in the DL/NN world,
written by leading figures, that lament the lack of part-whole
relationships in DL/NN.  (Hinton?)

---------------

# Slide 5: Jigsaw Paradigm Established in Linguistics

This idea of assembly from parts is not new; it appears explicitly in a
figure in a paper from 1991, and implcitly in earlier work (when you
know what to look for).

It is repeatedly discovered by linguists. One (EA Nida) spoke of assembling
words into sentences the way that chemists assemble atoms into molecules.

Bottom of the slide shows how to bridge from this approach to a
statistical approach.  It shows a maximum spanning tree parse,
wth te weights indicated in the links.

Of course, there can be other links as well: just the dominant ones are
shown.

---------------

# Slide 6:  Provides Semantics for Symbolic AI

This slide applies the jigsaw connector paradigm to find things that can
be seen -- because telescopes, lenses and eyes all have similar jigsaw
relationships to the perceived object.

Lingusits have long observed that "pure syntax" alread encodes the
shallower layers of semantics.  You can get the "meaning" of words
simply be seeing how they are "connected" to ther surroundings.

---------------

# Slide 7: Not Just 1D, 2D, 3D, but also Abstract Sensory Domains

This idea works in general sensory domains.

Here's a clip of a whale song recorded by NOAA.

It has a syntactic structure that can be represened via jigsaw
conectors.

Not that there are at least five dimensions here:
frequency, intensity, time, shape, chirp modulation.

I've oversimplified the segmentation to make it fit in this chart.
The true structure will be more complex.

---------------

# Slide 8: Segmentation and Tokenization as (evolutionary, ML) Program Learning

The point here is that this jigsaw paradigm is not pie-in-the-sky
theory.  There are practical algorithms that can extract this structure.

They are old-school. Conventional machine learning.

Can DL/NN be adpated to this? Maybe. I've got some wild ideas.
No one has done this.

---------------

# Slide 9: Experimental results

I've personally walked part-way down this path, creating software to do
all this. It's slow-going.

Perhaps the most remarkable result is this Gaussian (Sorry, if you were
expecting a LaMDA-like large language model)

This Guassian, a "Bell Curve", shows the distribution of the MI
between pairs of jigsaw puzzles.

It's remarkable because Gaussians indicate that the underlying data is
uniformly distributed on the surface of a high-dimensional
(projective) sphere.

In physics, this is called a "spin glass".  There is a brand new, very
young branch of mathematics that studies probability in high dimensions.

It's nothing at all like the low-dimensional probability theory of
college textbooks. It's a brand-new world.

I find it remarkable that this is a kind of experimental demonstration
that the English language is maximially efficient in it's use of
grammatical relationships.

---------------

# Slide 10: Common Sense as Inference over Symbolic Domains

I'd like to conclude with a conjecture, a bit of science-ficition,
as it were.

The roadway I've sketched appears to be recursive. The tricks can be
replied repeatedly, to mine deeper into the semantic direction.

This is a panpsychic view of semantics: if everything is a graph,
if semantics arises (purely) from relationships, then "all that we have
to do" is to mine the relationships ever more deeply. The jigsaw
paradigm provides a practical tool, both theoretically and software-wise
in which this can be performed.

---------------


Image Grammar
=============
Can the ideas here be applied to images? Of course they can. Here's a
short sketch.

Motivational Anecdote
---------------------
An anecdote from Hanson Robotics Sophia the Robot. She had this habit of
trying to talk through an audience clapping. Basically, she could not
hear, and didn't know to pause when the audience clapped. (Yes, almost
all her performances are scripted. Some small fraction are ad libbed.)
A manual operator in the audience would have to hit a pause button, to
keep her from rambling on. So I thought: "How can I build a clap
detector?" Well, it would have to be some kind of audio filter -- some
level of white noise (broad spectrum noise), but with that peculiar
clapping sound (so, not pure white noise, but dense shot noise.)
Elevated above a threshold `T` for some time period of `S` at least one
second long. It is useful to think of this as a wiring diagram: some
boxes connected with lines; each box might have some control
parameters: length, threshold, time, frequency.

So how do I build a clap detector? Well, download some suitable audio
library, get some sound samples, and start trying to wire up some
threshold detector *by hand*. Oooof. Yes, you can do it that way:
classical engineering. After that, you have a dozen different other
situations: Booing. Laughing. Tense silence. Chairs scraping.  And
after that, a few hundred more... it's impossible to hand-design a
filter set for every interesting case. So, instead: unleash automated
learning.  That is, represent the boxes and wires as Nodes and Links
in the AtomSpace (the audio stream itself would be an AudioStreamValue)
and let some automated algo rearrange the wiring diagram until it finds
a good one.  [(See README-Audio.)](./README-Audio.md).

But what is a "good wiring diagram"? Well, the current very
fashionable approach is to develop a curated labelled training set,
and train on that. "Curated" means "organized by humans" (Ooof-dah.
humans in the loop again!) and "labelled" means each snippet has a
tag: "clapping" - "cheering" - "yelling". (Yuck. What kind of yelling?
Happy? Hostile? Asking for help? Are the labels even correct?) This
might be the way people train neural nets, but really, its the wrong
approach for AGI. I don't want to do supervised training.  (I mean, we
could do supervised training in the opencog framework, but I don't see
any value in that, right now.)  So, lets do unsupervised training.

But how? This requires a conceptual leap. This leap is hard to explain
in terms of audio filters (its rather abstract) so I want to switch to
vision, before getting back to audio.  For vision, I claim there
exists something called a "shape grammar". I hinted at this in the
last email.  A human face has a shape to it - a pair of eyes,
symmetrically arranged above a mouth, in good proportion, etc. This
shape has a "grammar" that looks like this:

```
left-eye: (connects-to-right-to-right-eye) and
   (connects-below-to-mouth) and (connects-above-to-forehead);
forehead: (connects-below-to-left-eye) and
   (connects-below-to-right-eye) and (connects-above-to-any-background);
```

Now, if you have some filter collection that is able to detect eyes,
mouths and foreheads, you can verify whether you have detected an
actual face by checking against the above grammar. If all of the
connectors are satisfied, then you have a "grammatically correct
description of a face".  So, although your filter collection was
plucking eye-like and mouth-like features out of an image, the fact
that they could be arranged into a grammatically-correct arrangement
raises your confidence that you are seeing a face.
[(see footnote 1)](#footnote-1).

Those familiar with Link Grammar will recognize the above as a peculiar
variant of a Link Grammar dictionary.

But where did the grammar come from? For that matter, where did the
eye and mouth filters come from? It certainly would be a mistake to
have an army of grad students writing shape grammars by hand. The
grammar has to be learned automatically, in an unsupervised fashion.
... and that is what the opencog/learn project is all about.

At this point, things become very highly abstract very quickly, and I
will cut this email short. Very roughly, though: one looks for
pair-wise correlations in data. Having found good pairs, one then
draws maximum spanning trees (or maximum planar graphs) with those
pairs, and extracts frequently-occurring vertex-types, and their
associated connectors. That gives you a raw grammar.  Generalization
requires clustering specific instances of this into general forms.
Some of this code is already written in a quasi-generic fashion.
Some is being developed now. A number of stages beyond the present
are envisioned, but remain distant.

The above can learn (should be able to learn) both a "shape grammar"
and also a "filter grammar" ("meaningful" combinations of processing
filters. Meaningful, in that they extract correlations in the data.)

That is the general idea. The rest of this file fleshes out the above
in slightly greater detail.

Shape Grammar
-------------
Lets presume a segmented image -- one where large blocks of similar
pixels have been identified, where some edge detector has drawn
boundaries around these blocks. This is straightforward, and image
segmentation can be found in conventional image processing libraries.
[(see footnote 2)](#footnote-2), [(footnote 3)](#footnote-3).

Given a segmented image, the goal here is to develop a grammar for it.
How might this work? Lets consider a grammar for a human face: the
forehead is above two eyes, a nose in between, a mouth below.  Let the
connectors be the compass points `N`,`E`,`W`,`S`, and the connection
rules that `N` can only connect to `S`, and `E` to `W`. A pair of eyes
then has the grammar
```
 eye: E or W;
```
This says that there must be two eyes (not one) and that they must be to
the east and west of one-another, and never above or below.  This is
where we can beat ordinary neural nets, which do not know about the
part-whole relationship, and do not know about relative positions.

How about the nose? Some fancier connectors are called for. Let `SWn`
be the connector saying "must have nose to the southwest".  Then:
```
eye: (E & SWn) or (W & SEn);
nose: NWn & NEn;
```
says that eyes and nose form a triangle. The mating rules here are a bit
awkward: so `SWn` can attach to `NEn`. We can partly fix this by using
`+` and `-` to mean "opposites", so that `S-` is the same as `N+` and so
`SWn+` is the same as `NEn-`. Whatever. Yuck, confusing either way.  For
lips, we'd have
```
eye: (Ee & SWn) or (We & SEn);
nose: NWn & NEn & Sp;
lips: Np;
```
and so on, for cheeks, forehead, *etc*. This is how one might construct
a grammar for 2D faces.

Given a segmented 2D image, can we "parse" it, *i.e.* apply the above
grammar, and thus identify where the eyes, nose, lips are? Or perhaps get
back the answer "no visible face here"?

Grammar Learning
----------------
There is a converse grammar-learning problem. Suppose we have five
thousand segmented images containing American flags. These will have
alternating red and white stripes, arranged vertically (usually). We can
compute pair-wise mutual information for segments, and this should give
a strong signal for "red segment is next to white segment". Such pairs
of segments will have a high mutual information (MI) between them.

Consider now the connectivity graph of all segments: this is the graph
where we put a dot at each segment, and an edge whenever two segments
touch.  The edges are weighted by the MI between them. High MI means
they co-occur, low MI means "irrelevant". Consider now an American
flag on a large variety of backgrounds. Segmentation will create edges
between flag-parts and back-ground, but all those will have low MI,
because the background will be random. Thus, we can build a spanning
graph that consists only of high-MI edges. This will be then be a
flag-recognizer, and it will enumerate the parts of a flag. (To keep
it easy, maybe start with a tricolor - e.g. a French flag).

Now, the above may sound naive, and maybe some grad student already did
this 20 years ago, and discarded it for something else. However, I bet
the grad student did not know about Link Grammar, and did not realize
the power of high-MI spanning graphs. So I think this is what is novel.
This is the thing no one has done before.

A Simpler Example
-----------------
The above examples raise some confusing questions regarding the
detection of features (facial features) or the segmentation of images
(decomposing a flag into component parts). These can be resolved with a
simpler example: the detection of outdoor scenes with a blue sky
overhead.

This detection problem can be decomposed into two parts: a vertical
gradient, and a color filter for the upper part of that gradient. These
two components in turn may be relativiely "primitive", or they may also
have a complicated structure. The vertical gradient may need to tolerate
a horizon that is not centered in the middle of the picture. This
suggests the gradient should be modeled by a low-order wavelet filter.
The color filter must be applied before the gradient filter, as there
might not be a sharp gradient in the hue or saturation of the image.

The gradient filter may need to tolerate trees or buildings or mountains
disrupting the horizon.  Perhaps a simple blur filter is enough to
accomplish this. But perhaps some more complex filter sequence can be
used: perhaps in combination with a edge filter to detect sharp vertical
and horizontal lines (city buildings) or with speckles (trees with
visible leaves).

Likewsie, the blue filter might be just a simple color filter, or it
might be some rather complex arrangement recognizing a complicated
region of the hue-saturation-value (HSV) space, or the RGB space, or
some other color-coordinate space.  There is no particular reason to
limit the color filter to only work on colors: it might also interact in
some non-trivial way with special filters like laplacians or edge
dectors.

The overall goal is **not** to engineer these filter sequences by hand,
but rather, to have the system learn them automatically. Thus, although
our intuitive notion of a "blue sky filter" is just a sequence of a blue
filter, a blur filter, and a gradient filter, this might not be what the
system actually learns! It may learn something more complex; it may
learn something more clever. It might learn something more fragile, or
something more robust.


Atomese
-------
The AtomSpace uses [Atomese](https://wiki.opencog.org/w/Atomese) to
represent knowledge graphs. An example knowledge graph for a sequence of
image processing filters might look something like this:

```
(GreaterThanLink (Number 0.5) ;; Select blue values
   (HueFilterLink (Number 0.0 0.0 1.0) ; RGB - red green blue
      (HaarWavelet (Number 0 1)  ; Lowest order Haar in vertical direction
         (VariableNode "x")))) ; Binding to input.
```
It might be convenient to encapsulate the entire tree in a
[`LambaLink`](https://wiki.opencog.org/w/LambdaLink_and_ScopeLink).
At this time, the AtomSpace does implement the
[`GreaterThanLink`](https://wiki.opencog.org/w/GreaterThanLink)
but not the `HueFilterLink` nor the `HaarWaveletLink` - these would
need to be created and attached to an image processing library.
Presumably, OpenCV would be the canonical choice. The Atoms themselves
should probably be shaped to closely resemble the natural OpenCV API.

It is envisioned that video and audio sources would inherit from
[`StreamValue`](https://wiki.opencog.org/w/Value), and that video and
audio samples from specific points in the processing pipeline could
be samples with StreamValues. Details are TBD, StreamValues are
currently an experimental part of the AtomSpace.

The reset of this text repeatedly talks about segmentation. In fact,
segmentation might not be the best approach: it might only be useful
for initial bringup and proof of concept. If it is used, then the
corresponding Atom migh be called `ConvexContingousRegionLink` or
something like that.

Practical matters
-----------------
The above seems doable with present-day CPU's and present-day
image-processing libraries. Segmentation is not that hard. Creating
spanning graphs is not that hard. Doing this for hundreds of thousands
of graphs shouldn't take longer than a CPU-day (my rough guess).
Computing mutual information from such pair-wise segment relationships
is not hard; it does require writing new software.

The images are reprocessed a second time, so as to obtain spanning
graphs with low-MI edges discarded. What remains are the skeletons of
the part-whole relationships in the image. These skeletons are readily
busted up into disjuncts.  Collecting statistics on the disjuncts, one
then readily obtains vectors, and so a grammar. Le Voila!  The same Link
Grammar as used for natural language can also be applied to visual
part-whole image segmentation!

Channels and the Channel Grammar
--------------------------------
The above sketch glossed over a deep and difficult issue that is far
more complex and abstract. For initial bringup and proof of concept, it
can be ignored; for a general theory, it is fundamental and unavoidable.

The problem with image segmentation is that colors are not uniform, but
vary in brightness and hue. To segment an image, one must make various
ad hoc assumptions, and, indeed, there are many rather complex
algorithms published for segmenting images.  These algorithms are
described in terms of filters and operations applied to an image.
An example can be found in [this tutorial from
OpenCV](https://docs.opencv.org/3.4/d2/dbd/tutorial_distance_transform.html):
the algo is to first sharpen, then gray-scale, convert to binary, apply
a distance transform, threshold, finally watershed.

The ultimate goal of unsupervised image learning is to learn this
algorithm, as well as the shape grammar. Consider splitting an image
into hue and saturation. The hue is a "channel of data": it is a
specific stream of data that resulted from applying the "hue transform"
to the input stream.  Image processing can be reduced to the idea of
applying transformations to input channels, creating output channels.
Image segmentation is then some specific sequence of transforms applied
to the input data channels.

Unsupervised learning requires not only a search of the 2D segmented
space, and a discovery of the shape grammar therein, but also a search
of the much higher dimensional space of possible filter sequences that
can be applied to the raw inputs. One is looking not only for meaningful
arrangements of shapes in a 2D image plane; one is looking for a
meaningful arrangement of transform filters in a processing pipeline. It
is the combination of these that must be learned.  Thus, the goal is not
to find 2D or 3D shapes or shape grammars, but to find N-dimensional
shapes, where N is the collection of channel transformations. This is
not a Euclidean N-dimensional space: it is the result of a hierarchical
combination of filters. The learning occurs in this abstract space.

However, for initial proof-of-concept, it is best to avoid the above
complexity. The proof of concept needs to show that shape grammars
exist, and that a reasonable pipeline can be built to find them.

Unsupervised Learning of Channel Grammars
-----------------------------------------
How might one learn a directed tree of image-processing operations, with
each vertex in the tree controlled by several adjustable numeric
parameters? In an earlier example, a tree of this form was offered up as
a concrete example:

```
(GreaterThanLink (Number 0.5)
   (HueFilterLink (Number 0.0 0.0 1.0)
      (HaarWavelet (Number 0 1)
         (VariableNode "x"))))
```
The general form is then
```
(OperationA (Number N)
   (OperationB (Number K L M)
      (OperationC (Number P Q)
         (Variable "x")))
   (OperationD (Number F G)
      (Variable "y"))
   ...)
```
Such trees would be constructed randomly, with preference given to
simple trees, at first. Parameters can also be chosen randomly, with
either uniform weightings, or some other distribution.

A preliminary collection of such filters to be developed at
https://github.com/opencog/vision -- Its currently at version 0.0.0.

The Opencog [MOSES](https://wiki.opencog.org/w/Meta-Optimizing_Semantic_Evolutionary_Search)
(Meta-Optimizing Semantic Evolutionary Search) documentation spells out
in considerable detail how a pool or "deme" of useful program trees can
be managed and evloved. The [ASMOSES](https://github.com/opencog/asmoses/)
project is a port of MOSES to explicitly use the AtomSpace for the
storage to the deme pool of program trees that are being explored.
It would need some work to extract the pool-management part of that
software from the rest of the infrastructure, which this project would
NOT use. The point here is that the generation and management of program
trees is not new, and that we already have some existing software to
handle this.

Examples of channel grammars that a working system should be able to
learn automatically:

* It should be able to learn the Cascade Classifier. For example:
  https://docs.opencv.org/4.5.3/db/d28/tutorial_cascade_classifier.html
  That is, instead of using the OpenCV cascade classifier, it should
  be able to learn the cascde classifier algo, on it's own.


The Learning Pipeline
---------------------
The learning pipeline for images, as I currently envision it, would work
like so:

Create `N=50` to `N=500` random filter sequences. Given a single image,
each filter sequence produces a single-bit `t/f` output. Given one image
and `N` filters, there are `N(N-1)/2` result pairs. If both ends of the
pair are `t`, then the count is incremented for that pair; otherwise
not.

Given `M` input images, apply the above to each of the images. The
result is a collection of pairs, with varying pair-counts. (Up to a
maximum of `M`. The bigger the `M`, the better is the general rule).
Given this raw info on pairs, the generic learning pipeline kicks in,
and does the rest.  The generic pipeline computes the mutual
information of the pairs, it extracts disjuncts, it merges disjuncts
into classes, and ... whatever will come next.

There are two aspects that are different with the image pipeline, as
compared to the language pipeline. One is that some of these random
filters may be generating useless noise. These are presumably those
with the lowest marginal MI. They need to be discarded, and replaced,
so that we build up a good collection of "useful" or "meaningful"
filters. The other is that the filters with the highest MI with
each-other might in fact be nearly identical, and so we only need one
of these, not both. One of the two needs to be discarded. Perhaps the
theory of matroids will be useful, here: We do not need to find a set of
vector basis elements, we just need to find something that is
"independent enough".


The Competition: Neural Nets
----------------------------
The conventional setting for AI these days is the deep-learning, neural
net industry. Competition, as such, seems absurd: these systems have
billions of CPU-hours behind them, computed with well-written,
well-debugged, highly optimized software, created by armies of salaried
PhD's working at the big tech companies. Any results achieved here will
look pathetic by comparison.

What is being proposed here is very different. Roughly speaking, neural
net systems employ a very shallow representational system: they attempt
to classify each and every *pixel* in an image, assigning it to an
object. This makes learning difficult, as most images have a huge number
of pixels in them. The shallow representation, of x,y,rgb is also an
impediment to learning: relationships between different pixels in the
image are cryptically encoded in the weight matrix. Perhaps that weight
matrix is encoding some combination of well-known image processing
primitives, such as laplacians, blurs and edge detectors, but there is
no way of knowing what those might be, or if they are even there.

To put it bluntly: the neural net systems are pixel analysis systems.
They are assigning probabilities to pixels; they are not examining
prepositional relationships such as above, below, next to, inside of,
beside, behind. They are not looking at structural co-occurances: houses
always have doors. Automobiles always have wheels. Dogs and cats always
have faces, bodies and legs. This prevents them from aiming at the
ultimate spatial reasoning question: "will the piano fit through the
door?" In effect, the pixel-driven neural net systems don't actually
"think", they don't need to. They don't need to find relationships out
of thin air. By contrast, this project searches for non-pixel-based
spatial relationships as the very first order of business.  So I think
this is something brand new that we're doing that no one else does.

Another key difference is that we are working explicitly at the
symbolic level. By having a grammar, we have an explicit part-whole
relationship. This is something the neural-net guys cannot do (Hinton,
I believe, has a paper on how one day in the distant future, neural
nets might be able to solve the part-whole relationship problem. By
contrast, we've already solved it, more or less from day one.)

We've also "solved" the "symbol grounding problem" -- from day one.
This is another problem that AI researchers have been wringing their
hands about, from the 1960's onwards. Our symbols are grounded, from
the start: our symbols are the filter sets, the grammatical dictionary
entries, and we "know what they mean" because they work with explicit
data.

Symbolic representations are important, because they allow traditional
theories of symbolic reasoning to be applied. One can apply logic:
Aristotelian logic, predicate logic, common sense logic. Careful,
though: the ultimate goal of a sparse symbolic network is to learn the
laws of logic itself.  That is, if one observes a correlation, that when
one does this, that happens, one is in fact learning a bit of
common-sense inference.  That is, common-sense inference is what
you can do after you've spotted a pattern in the sparse network of
symbols. Unsupervised learning has to be a project of spotting patterns
in sparse symbolic networks. Common-sense reasoning is then the
application of those learned patterns to the prediction of outcomes.

Another very old AI problem is the "frame problem", and I think that
we've got that one licked, too, although this is a far more tenuous
claim. The "frame problem" is one of selecting only those things that
are relevant to a particular reasoning problem, and ignoring all of
the rest. Well, hey: this is exactly what grammars do: they tell you
exactly what is relevant, and they ignore the rest. The grammars have
learned to ignore the background features that don't affect the
current situation.

But whatever... This gets abstract and can lead to an endless spill
of words. I am much more interested in creating software that actually
works.

Perhaps deep learning and neural nets will someday be able to do this.
Today, they cannot. The algorithms described here currently seem to have
a fundamental advantage over neural nets.  Whether or not this
fundamental advantage can be turned into practical, high-speed code
remains to be seen. We're starting from far behind, but maybe we can
shoot far ahead.


How does this work?
-------------------
It is often said that no one is quite sure of why neural nets work so
well (see however, paper by XXX).  The situation here is very different.
Consider the image grammar for a stoplight. If you are a human, then
it is "obvious" what the part-whole relationship is. One can write down
a passable grammar for this in just a few minutes: glowing red above
yellow above green, surrounded by a painted yellow or black harness.
This is "obvious", and detecting this in images seems like it should
be pretty easy.

This is in very sharp contrast to what neural nets do: when a neural
net picks out a stoplight from an image, no one is quite sure how it is
doing that.  Perhaps somewhere in there are some weight vectors for red,
yellow, green, but  where are they? Where are they hiding? How do neural
nets handle part-whole relationships?  There is a paper (from Hinton?
XXX find and reference this) stating that the part-whole relationship for
neural nets is the grand challenge of the upcoming decades.

By contrast, the part-whole relationship for grammars is "obvious".

What is this good for?
----------------------
> The final big question is what can you really do after you get that
> grammar? What sort of inferences? How useful they are?

Well, for starters, if the system recognizes a stop light, you can ask
it: _"how do you know its a stop light?"_ and get an answer: _"because
red above yellow above green."_ This ansswer is obtained by direct
examination of the primary structure of the filter set, which has
explicit red, yellow, green filters in it, and the examination of the
explicit image grammar, with above-below connectors explicitly present.

You can ask _"and what else?"_ and get the answer _"on a painted black
or yellow background"_ -- Again, this info is explcitly available in the
program tree that represents the stoplight.

Continuing:
 * _"Q: And what else?"_ _"A: The colors glow in the dark"_
 * _"Q: and what else?"_ _"A: They are round"_
 * _"Q: And what else?"_ _"A: only one comes on at a time"_
 * _"Q: And what else?"_ _"A: The cycle time varies from 30 second to
    three minutes"_
 * _"Q: What is a cycle time?"_ _"A: The parameter on the time filter
    by which repetition repeats"_
 * _"Q: What do you mean by round?"_ _"A: The image area of the light
    is defined via a circular aperature filter".

Good luck getting a neural net answering even one of those questions,
never mind all of them.

Symbolic representations are useful not only in narrow situations like
the above, but also in broad questin situations:
 * _"Q: "what else is round?"_ _"A: The sun, the moon, billiard balls,
   bowling balls, baseballs, basketballs".

I think we are very very far away from having a neural net do that
kind of question answering. I think this is well within reach of
grammatical systems.

Associations between symbols and the things they represent is the
famous "symbol grounding problem", considered to be a very difficult,
unsolved problem in AI. I'm sketching a technique that solves this
problem. I think this is unique in the history of AI research. I don't
see that anyone else has ever proposed a plausible solution to the
symbol grounding problem.

Performance
-----------
There are two distinct stages: learning a given filter sequence and
image grammar, and applying what has been learned.

There should be no issue with applying previously-learned filter trees:
1980's-era DSP could do image processing quite well. Modern multi-core
CPU's and GPU's are capable of applying filter sequences to video feeds
without much difficulty.

The learning phase has two steps to it. Step one: Can we get it to work,
at any speed? (I think we can; that's the point of this essay.) Step two:
can we get it to work fast? Who knows -- compare this to deep learning,
which took decades of basic research spanning hundreds of PhD theses
before it started running fast. The motley crew of individuals who might
appear for this project are not going to replicate a few thousand
man-years of basic research into performance. At this point, making it
run fast is subordinate to the more interesting problem of making it do
something that has never been done before.


Development Plan
================
This section describes the initial steps that can be taken, to validate
the above ideas.

Status
------
**Version 0.0.1** - The core ideas have been sketched out (this README).
Almost no vision-specific code has been written; see however the
[opencog/vision](https://github.com/opencog/vision) git repo.

Corpora
-------
Collect a suitable corpus of images. For proof-of-concept, the images
should be rather simple, and relatively uniform, so that the
commonalities can be easily identified. Ideas include:

* Different views of a flag of one country. Perhaps a tricolor (German,
  French flags) is a good place to start. More challenging might be the
  American flag, or the Union Jack, or others.  The goal here would be
  to learn the order of the colors, to learn the ratio of dimensions
  (width to height, ratio of dimensions of other feature sizes) and to
  distinguish the flag from the background.  Bonus points: distinguish a
  flagpole, if present.  If most of the pictures feature blue sky as
  background, then distinguishing sky from flag might be problematic.

* Different views of stop lights, from fairly close-up. The goal here is
  to learn that red is always above yellow is always above green, that
  the lights are circular, that only one light is on at a time, that the
  frame is rectangular, and everything else is background.  Bonus points:
  distinguish the supporting poles, if present. If most of the pictures
  feature blue sky as background, then distinguishing sky from the light
  might be problematic.

* The GTSRB (German Traffic Sign Recognition Benchmark). Images are in
  43 classes. A total of 52,839 images.

* Different views of stereotypical suburban homes. The goal here is to
  find doors, windows, roofs.  Bonus points: walkways, lawns, shrubbery,
  fences. This is considerably more difficult than the flag problem, and
  should not be tackled until the proof-of-concept works for flags.

* Geometric objects in simple spatial arrangements. A sutiable,
  ready-to-go collection is the DeepMind
  [Multi-Object Datasets](https://github.com/deepmind/multi_object_datasets)
  Of interest is the "Objects Room". This consists of a simple 3D room,
  with six different geometricly-shaped objects placed in random
  locations. Since they are in random locations, there is no particular
  structure to the image, other than the be background is "punctured" by
  the objects. Since the objects have distinct shapes, the grammar
  will presumably learns to identify the shape of the object. Of course,
  the collection of image processing primitives will need to include
  filters that are senstitive to shapes.

Basic Image Segmentation
------------------------
For training to work, images need to be segmented into patches of
roughly similar color or texture. For this, the simplest-possible
procedure should be used, as anything much fancier is "cheating".
The goal of segmentation is to provide reasonable approximations for
the following:

* Relative sizes of adjacent blocks, good to about one decimal place.
  Maybe an additional special size ratio "really tiny" or "really large"
  i.e. more than a 1 to ten ratio. This provides about 3 bits of info
  per pair of adjacent blocks.

* Relative colors of blocks: more red, more blue, more green, brighter,
  dimmer, very bright AKA white, very dark. Sharp contrast or low
  contrast.  This provides about 3-5 bits of relative color info per
  pair.

* Relative positions: above, below, left, right, four diagonal
  directions. This provides about 3 bits of relative position info.
  A special position might be "all around".  Perhaps a bit-mask -- e.g.
  "above and to the right."

* Rough geometry: triangular, round, square, oblong, rounded corners,
  sharp corners, star-shaped, sharp needle, obtuse, amorphous. Limit
  this to another 3 to 5 bits of approximate shape info.

Of course, more bits of info could be provided, but more bits means more
training time, requires a larger corpus, and, in general, makes the
learning steps harder. Dimensional reduction is a good thing. So, for
now, fewer bits.

The goal here is to find some off-the-shelf image processing software
that is just good enough to provide the above.  Anything fancier would be
overkill. The goal here is ease-of-use: it has to be easy to write code
that provides the above info for an image.

When analyzing photographs of tri-color flags, it's assumed that the
image processing will typically produce 2 to 10 contiguous blocks of
color. Ideally 4: the three bars, and the background. This seems
unlikely, though, in most cases.

Counting
--------
Create a table of attribute-pairs, and increment by one every time that
pair is observed.  For example, given two adjacent regions, if the
redder one is above the other one, then increment the count on (redder,
above) by one.  That is, there is a number N(redder, above), and it is
incremented each time that relation is seen. Likewise, N(redder,
triangular), and N(redder, bigger), and also N(above, bigger), if that
is the case.

For the above 4 attribute categories, each described by 3 bits, there
will be 4! x 2^3 x 2^3 = 24 x 8 x 8 = 1536 different possible pairings.

This is one reason to keep the number of attributes low, and the number
of bits per attribute low: the goal is to keep the "vocabulary" low.

These counts can be kept "anywhere", initially, but all of the rest of
the pipeline is built on the AtomSpace, so it makes sense to keep these
counts in the AtomSpace.  Ask me how to do this.  Its not hard, but
there is a bit of initial complexity. I'll help set this up. Just ask.


Pair-wise Mutual Information
----------------------------
Given pairs, compute the mutual information between the above
attributes. This project provides detailed tools for doing this,
presuming that you've kept the counts in the AtomSpace. Ask.

Minimum Spanning Tree
---------------------
From this point on, the same algo and code base as used for natural
language should be possible. The code will need a lot of careful
generalization, but maybe not a complete rewrite.

Worked example TBD.

3rd party tools
---------------
Some 3rd party tools that might be useful.

 * https://acav100m.github.io/ -- ACAV100M
   Automatic Curation of Large-Scale Datasets
   for Audio-Visual Video Representation Learning

Footnotes
=========

### footnote-1
This example is meant to sketch a general idea, but it raises a question:
"but where did the eye, mouth and nose detectors come from?" The answer
is that these are in turn built recursively out of other filter trees.
At no point does this require that some neural net be trained on eyes
and mouths. A simpler example might be the geometric structure of a
stop-light: red above yellow above green. That these can be detected
with relatively simple, straight-forward filter sequences can be taken
for granted. Details of how these can be constructed are presented
further into the text.

### footnote-2
This example is meant to sketch a general idea, but it raises a question:
"where does the segmentation come from?" The answer is that simple
segmentation can be built recursively out of more primitive filter trees.
At no point does this require that some neural net be trained to perform
image segmentation. A simpler example might be a filter that recognizes
the upper half of an image is blue. This is composed of two subfilters:
a vertical gradiant filter, and a color filter. It is the composition of
such primitive filters that yields more complex inputs for the detection
of features having greater complexities. The example is illustrative; in
the end, feature detection is performed by complicated assemblages of
primitive filters.

### footnote-3
Segmentation is not a process of assigning a label to every pixel. It
could be but need not be.  More properly, it is the identification of a
region of an image that can be assigned a particular label.  This text
talks about filters, and, for the most part, these filters are
envisioned to be pixel-size independent. That means that spatial filters
will be based on wavelet filters or something similar, allowing for
scale-independent, multi-scale image analysis.

Bibliography
============
* **MONet Algorithm**

  Burgess _et al._ [MONet: Unsupervised Scene Decomposition and
  Representation](https://arxiv.org/abs/1901.11390) arXiv:1901.11390
  (2019)

  The MONet algorithm describes images in terms of "attention networks"
  and "attention masks".  Roughly speaking, these masks are a set of 2D
  spatial filters identifying a region of pixels, and their color. The
  goal of training is to learn a collection of these masks. Each object
  in an image corresponds to a mask (the mask picks out the pixels that
  belong to the object). More precisely, each pixel is assigned a
  probability of belonging to a given object.

  This project is different: there is no desire to classify every pixel;
  for the most part this is irrelevant. To obtain a grammar, one need
  only assign the relative orientations and sizes and colors of objects,
  and not their absolute locations in the image. Furthermore, the
  grammar leverages abstract properties, such as texture and shininess,
  not just hue and saturation.

  The MONet algo appears to attempt to segment single images (snapshots).
  This project is also incapable of learning from only one image: it
  needs to train on a broad set of images of the same thing, in order to
  recognize the "sameness" of it, irrespective of lighting and background.

  That said, it is quite possible that some or many of the learned
  filter trees will resemble attention masks.  If this is the case, then
  it might be reasonable to use an algo such as MONet to find (some of)
  the relevant mask sets. That said, some simple blurring and flood-fill
  is probably sufficient for this project.

* **Attend-Infer-Repeat (AIR) algorithm**

  Eslami _et al._ ["Neural scene representation and
  rendering"](http://science.sciencemag.org/content/360/6394/1204)
  Science, **360**(6394) pp 1204–1210 (2018).

  The AIR algo factors an image into "what" and a "where" components.
  The "what" is described by a variational autoencoder (VAE). The
  "where" is provided by a spatial transformer module, which scales
  and shifts the "what" into the correct "where" location.

* **Spatial Transformer Networks**

  Max Jaderberg, Karen Simonyan, Andrew Zisserman, _et al._ "Spatial
  Transformer Networks."  Advances in Neural Information Processing
  Systems, pp. 2017–2025, (2015)

  Specific to the problem of scaling and translation in neural net
  models. Suggests how to deal with that, by including an explicit
  2D affine transformation during neural net training.

* **IODINE -- Iterative Object Decomposition Inference Network**

  Greff _et al._ ["Multi-Object Representation Learning with Iterative
  Variational Inference"](http://proceedings.mlr.press/v97/greff19a.html)
  Proceedings of Machine Learning Research PMLR 97 pp 2424-2433 (2019).
  JMLR

  Simultaneously segments an image into multiple components and
  identifies the components as objects. Uses VAE (variational
  autoencoders) to identify individual objects.  Neural net, pixel
  based.


Honorable Mention
=================
Things that are shapes, and are grammars. Really hasve very little to do
with (the short-term goals of) this project, but seem to be worth mentioning.

* **Shape Grammar**

  Mohamed Sobhy M. Ibrahim, ["Creative approach to design formulation
  Shape grammars as a tool in architecture design analysis and
  synthesis"](https://www.researchgate.net/publication/284899276_Creative_approach_to_design_formulation_Shape_grammars_as_a_tool_in_architecture_design_analysis_and_synthesis)
  Master's Thesis, Architecture, 2005, Beirut Arab University

  The idea of architectural grammars is an old one, and architects have
  been buzzing about this since at least the 1970's, if not earlier.
  This Masters Thesis provides a good review of "Shape Grammars":
  literally grammars, in the formal, and in the linguistic sense, that
  are about shapes: two-dimensional and three dimensional. The grammars
  are generally generative, and describe everything from Chinese lattice
  designs (Stiny, 1977) to floor plans of villas (Stiny, 1978).

* **Algorithmic Botany**, Przemyslaw Prusinkiewicz

  The [Algorithmic Botany](http://algorithmicbotany.org/papers/) website
  catalogs three decades of research into how grammatical systems
  express themselves in a botanical setting. This is exremely powerful
  stuff: it explores, in plain and direct terms, how generative grammars,
  specifically Lindenmayer systems (L-systems) generate the rough,
  overall structure of plants, and how they can be coupled to
  reaction-diffusion equations to obtain the fine details of shape and
  pigmentation. This captures the essance of what it is that DNA and
  proteins do in living systems, while effectively avoiding the
  complexities and mystifications of biochemistry.  If you want to
  understand what DNA is doing, from the systems standpoint, this is
  the way to do it.

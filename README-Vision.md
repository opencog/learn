
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
a good one.

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
[(see footnote 2)](#footnote-2).

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

Grammar learning
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

The Competition: Neural Nets
----------------------------
The conventional setting for AI these days is the deep-learning, neural
net industry. Competition, as such, seems absurd: these systems have
billions of CPU-hours behind them, computed with well-written,
well-debugged, highly optimized software, created by armies of salaried
PhD's working at the big tech companies. Any results acheived here will
look pathetic by comparison.

What is being proposed here is very different. Roughly speaking, the
neural net systems require supervised training. They don't actually
"think", they don't need to. They don't need to find relationships out
of thin air. So I think this is something brand new that we're doing that
no one else does.

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


Development Plan
================
This section describes the initial steps that can be taken, to validate
the above ideas.

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

* Different views of stereotypical suburban homes. The goal here is to
  find doors, windows, roofs.  Bonus points: walkways, lawns, shrubbery,
  fences. This is considerably more difficult than the flag problem, and
  should not be tackled until the proof-of-concept works for flags.

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
such primitive filters that yeilds more complex inputs for the detection
of features having greater complexities. The example is illustrative; in
the end, feature detection is performed by complicated assemblages of
primitive filters.


Image Grammar
=============
Can the ideas here be applied to images? Of course they can. Here's a
short sketch.

Image Recognition
-----------------
Lets presume a segmented image -- one where large blocks of similar
pixels have been identified, where some edge detector has drawn
boundaries around these blocks. This is straightforward, and image
segmentation can be found in conventional image processing libraries.

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

Practical matters
-----------------
To me, the above seems doable with present-day CPU's and present-day
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

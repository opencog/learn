
Image Grammar
-------------
Can the dieas here be applied to images? Of course they can. Here's a
short sketch.

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
and so on, for cheeks, forehead, **etc**. This is how one might construct
a grammar for 2D faces. 



Notes about Theory
------------------
This file documents some overview notes. I have previously attempted
to keep a diary in the [learn-lang-diary](learn-lang-diary) directory,
but the flood of content there obscures what might be some important
points.

### Structural equivalence and optimization
There's a tangled set of issues that arise when one asks "what is the
structure of something?" Describing the structure of something is not
unique, of course. One may have a photograph of a house, or the
blueprints for it; both provide valid structural descriptions. In
protein chemistry, one has the primary, secondary and tertiary
structures: the sequence of amino acids, the sulfur bonds, and the
folding. In software, one has the code, and the pseudocode.

In software, one has the very important question of "what does this
structural element do?" In assembly language, one has a machine
description: some particular insn will read these registers, write to
those, and set some bits in some flag-word. Compiler optimization is all
about having an accurate machine description, and then exploring the
space of combinatoric rewritings of machine instructions that will yield
the same effects. That is, find some other sequence of instructions that
has the same net effect, but is shorter or runs faster or uses less
memory. This is sometimes called "refactoring": rewriting code, without
altering its function. In mathematics, this is called a "homotopic
deformation": the start and endpoints are equivalent; the path is
different. If one adds execution time (or RAM usage) as a metric, then
this imposes a metric space structure onto the space of homotopic
deformations. This in turn allow one to define a principle of least
action, and ask for the geodesic (shortest path) between two points.
That is, having a metric allows a derivative to be defined, and thus (in
principle) have Euler-Lagrange equations. In practice, this is
impossible, as the space is highly discontinuous. Machine learning
systems can attempt hill climbing, but famously get stuck in local
minima/maxima.

But we're getting ahead of ourselves. The above can come only after some
preliminary structure has been discerned. Homotopic deformations can only
come after several different structural variations are assigned to a
given object, together with some judgement of whether there is some
fine-grained ("continuous", in the Zariski topology (etale topology??))
path (homotopic deformation) from one to the other. In proof theory, the
concept of continuous deformations are given by the set of inference
rules viz, given premise P we can deduce Q via the rule P->Q. That is,
an inference rule defines a single valid "homotopically continuous"
deformation from P to Q.  But, of course, inferences are structural
rewrites. Inference rules are rewrite rules; this is the Curry-Howard
correspondence.

This suggests that the discernment of structure in nature can be broken
into two parts: observing (finding) a structural representation, and
then finding structural morphisms. That is, finding maps to other
structural representations that can be said to be "equivalent" in some
sense. Well, and then there is a third problem: that of finding or
"optimizing" the representation for some particular quality.

Several examples of "optimization" are due. There are several ways to
"optimize" a Sudoku puzzle. One is to find the solution. This is
"optimal" in that there is no shorter solution of that puzzle. Another
conception is to find the shortest path to that solution. This is
optimal, in that there is no shorter path from start to finish.
Another way is to find an algorithm that generates the shortest path,
or perhaps an algorithm that generates some reasonably-short path.
A fourth sense occurs as a meta-problem: in the space of all possible
Sudoku-solving algorithms, how do we find the one that generates the
best possible paths? Or, at least, generates reasonably good ones on a
statistical ensemble of all possible Sudoku puzzles?

This fourth item in the paragraph above becomes, again, as
structure-discernment problem, reified. What is the space of
Sudoku-solving algorithms, and how can that space be described,
represented, characterized and operated upon? Obviously, there is an
infinite regress here; however, at each step of the game, there is a
fairly urgent desire to have optimal representations and optimal
algorithms for working with them.

Thus, a part (a large part?) of the task here is to obtain, create and
work with reasonable algorithms that can do optimal things, without
taking too long to get something done. Of course, humans have discovered
many such: the greedy algorithm or the Davis-Putnam DPLL algorithm.
The question now is "how can I automate that discovery?"

There are several answers. One is try to stick to a purely symbolic
approach: write some solver that enumerates all possible cases, in some
ergodic odometer style, and examine each case. Then if we are lucky,
perhaps we can DPLL prune some of the branches off that ergodic
odometer, as we now know that "there is nothing there that is
worthwhile". The ability to prune gives a superior algo. Then what:
rinse and repeat?

An issue that arises when seeing a new structure for the first time is
"have I seen something similar before?" and "what tricks was I able to
apply last time?" This requires memory -- both working memory and
long-term memory. This also requires query: several questions are asked
above; those questions are to be applied as queries to memory. The
structural representation has to be such that it is effectively
queryable, and, better yet, optimally queryable. It is no accident that
"query planning" and "query optimization" were a big deal in 1990's Big
Data systems, and that Google was founded on a shift-reduce algorithm
that solved the Markov chain that we called "the Internet". The message
here is always "reify" and "reify again".

Another issue that arises is that of "percolation". If one tries to
explore (enumerate) a space whose size explodes combinatorially with
the number of steps taken, and the size of that space is infinite, then
one will have a "Turing machine that never halts": you'll just wander
off, quasi-ergodically into some corner, random-walk-style, never to
return to the origin to explore other possible paths. There are several
algorithms for dealing with this. One is to do breadth-first search,
instead of depth-first. Another is to apply a DPLL-style transformation,
and prune the finite branches that don't go anywhere (and so avoid
exploring them entirely).

But how does one know if a branch is finite or not? There might be a
small and narrow and very thin tube that connects it to some other vast
network. Like two giant cave systems connected by a small passage, you
could wander around in one cave system for a very long time, failing to
discover the path to the other.

Well, so in this very round-about way, we arrive at the idea of using
LLM's to search for and discover optimal paths and structures and
algorithms. These work by representing structures with weighted tensor
structures. Again, a structure: e.g. a transformer is a very specific
wiring diagram that connects up a collection of tensors (well, actually
functions, since the sigmoid is non-linear) in a very certain way, such
that vectors flow across this "wiring diagram". By preparing a training
corpus, one can train this NN so that the weights capture some aspect of
the structure in the training corpus. In this case, the idea of structure
is "Bayesian": the corpus consists of the likely samples, and the
unlikely ones are never there, never occur. They are, in some respects,
given a uniform distribution. In the percolation example above, an NN
trained on a cave system would assign equal probability of finding the
"hidden passage" everywhere; the hidden passage could be anywhere in the
cave system; we don't know where. It is(?) (should be?) given
equiprobability.

The problems associated with using LLM's are now well known. One is that
they "hallucinate". More mechanically, one could say that they are
"blurry", and perceive two different structures as being the similar,
when they are not, really. This is a vector-space embedding problem, a
pixelization problem. The floating-point nature of the embedding allows
bleed-through of unrelated things via small perturbations of the
weights. Its "blurry", that's all.

A second issue of LLM's is the general lack of working memory. The
context window of a chat session provides the working memory for some
given chat, but if you then ask the LLM to RTFM, the context window
quickly blows up, using tens of thousands, hundreds of thousands of
tokens. At this size, it struggles to continue to "think" coherently.

A third issue seen in LLM's is the inability to adequately apply recall
to solve problems. For example: LLM's are trained on hundreds of
thousands of scientific papers, and, if you prompt them for some
scientific factoid, the will recall it with very high accuracy.
Breathtaking, even: the scope of knowledge is amazing. But if you then
ask it to solve some technical problem that could be solved by applying
one of these scientific facts, it will utterly fail. Instead, you'll get
something that sounds like it was cribbed from a textbook in the 1970's
or the 1980's. Why is that? Well, because the representational embedding
is such that two closely-related scientific facts are very far from
one-another, and the system, of course, cannot find the path from here
to there: its simply too far away. The textbook solution, however, is
highly clustered. Every scientific paper reviews the textbook solution
in its introductory paragraphs. This extreme repetition allows the LLM
to localize this into a distinct cluster. When you ask for a solution,
it finds the cluster.

The fact that the LLM can operate at a high-school or college level
clustering of textbooks solutions is certainly quite wonderful. The
inability to perform recall to obtain related ideas without stringent,
careful prompting is infuriating.

A fourth issue is "thinking" or "reasoning". The training corpus has
examples of thinking and reasoning; and the LLM's can emulate this, but
only up to an extent. The blurriness problem results in confused
thinking, and sometimes complete logical short-circuits, break-downs.
The LLM perceives a homotopic deformation from one state to another when
there is none. it perceives two pieces of software as having equivalent
function, when they don't. It perceives a solution of a Sudoku puzzle,
when one of the constrains is clearly violated. This is the "blurriness"
issue.

Thus, we have identified several things:
* NNs offer very powerful representational systems that can discern
  structure in nature in an effective, efficient and fast manner.
* The vectorized nature of NN's has three obvious shortfalls:
  blurriness, memory and reasoning.

Recapping:
* Blurriness and bleed-through, because unrelated concepts are
  mapped close to one-another, and minor perturbations cause the
  one to be confused for the other.
* Lack of integrated memory. NN memory comes in two forms: the
  memory/knowledge burning into the weights, which is static, unchanging
  and determined by the training run, and the current location in the
  hyperspace, which was arrived at by pumping a bunch of tokens through
  the weight structure. This is lost, whenever a new session is started.
  And even then, part of this location is path-dependent, so during
  compaction, even though the location is not lost, older prior prompts
  that encoded vital facts are lost. (Example: I keep having to remind
  Claude to trim trailing whitespace. It keeps forgetting.)
* Inability to reason in a logical fashion, which drawing on a pool of
  crisp logical assertions. Since the representation system is not
  crisp (boolean true/false) the inferencing isn't either. The reasoning
  appears to be emulated: there are de facto reasoning styles embedded
  in the weights, e.g. reasoning by syllogisms, these aren't crisp and
  precise; rather they are paths through nearby locations in the
  hyperspace. They are "homotopic" only to the degree of blurriness that
  clouds judgements of equality (equivalence).
* Inability to perform adequate recall of interrelated ideas (without
  explicit prompting). This is a representational short-coming:
  inter-related ideas are embedded in such a way that there are no short
  paths between them, and so they cannot be found, unless the human
  already knows the path, and can guide the LLM. To put it differently,
  the LLM representation is never reified. A structural element is
  given a vector embedding, full stop. There is no "embedding of the
  embedding" that would give the "pseudocode representation" of the
  concrete specifics. I suppose this is a hot topic of research, but
  I dunno.

So a big chunk of this project is to attempt to work with and solve the
above limitations, by explicitly attaching a symbolic representation
system, Atomese, to the vector embeddings of NN's. How to create such
attachments is the magic question being wrestled with, here.

### Interfacing to LLM's
Initial attempts to interface with LLM's can be thought of as being a
for of "dynamic prompting". I, as a human, have to constantly guide the
LLM by creating prompts. Some of these are throw-away: I write them
once, and discard them. Others, the more permanent ones, I stick into
a file, and force the LLM to read the file every time (burning a bunch
of tokens in the process.)

The current design is a kind of "dynamic prompting" create a system that
reminds the LLM to go through some checklist, follow some procedures,
re-examine assumptions, try things a different way, run the unit tests,
RTFM, all of that. Its "dynamic" in that such prompts would be
auto-generated, contextually, depending on the problem at hand.

The other aspect of the design is to obtain these dynamic prompts by
issuing queries and deductive chains on the contents of the AtomSpace.
That is, some collection of queries (catalog or vector of queries) sits
in the AtomSpace, accessed, e.g. by DualLink, and get processed and
sorted by some daemon process/thread, to generate a dynamic prompt that
re-routes the LLM to go in a somewhat different direction than it was
going in, before.

At least, that is the current conception. Most of this work is happening
in a (currently private) sandbox, where Clause is making quite the mess
of immature, half-baked, incorrect and confused attempts, despite my
best efforts to lead it by the nose.

Part of the problem is that Claude does not really understand Atomese or
the AtomSpace, and so has great trouble coding with it. This causes me
to ruminate about axiomatic descriptions, specifically, axiomatic
descriptions of Atomese, indicating what the inputs are, what the
outputs are, what is transformed. Atomese is a lot like the insns of a
CPU insn set, are are meant to assemble and "snap together" just like
insns are assembled into a working program. Compilers can do this,
because they have (1) an accurate machine description and (2) a bag of
algorithms through which source code can be transformed into insns using
that machine description. Currently, Claude has neither of these. The
"machine description" of Atomese is informal: a bunch of wiki pages. I
could tell Claude to "read the source, Luke" but this offers at best a
very localized patch, and is insufficient at larger scales. It also
doesn't have a "bag of algorithms"; it has a collection of examples,
which it can reason from by analogy, but again, these are highly
localized. If the problem solution is not close to what can be found in
an example, Claude is lost again.

This once again highlights the difference between its training (vast
numbers of algos written in python, and baked into its weights) and what
it can inference on given some prompts.

The paragraphs above indicate why this is a hard problem, but they also
indicate that there seems to be a path through this mess.

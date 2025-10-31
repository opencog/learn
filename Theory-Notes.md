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
blueprints for it; both provide valid structural deescriptions. In
protein chemisty, one has the primary, secondary and tertiary
structures: the sequence of amino acids, the sulfer bonds, and the
folding. In software, one has the code, and the pseudocode.

In software, one has the very important question of "what does this
structural element do?" In assembly language, one has a machine
description: some particular insn will read these registers, write to
those, and set some bits in some flag-word. Compiler optimization is all
about having an accurate machine description, and then exploring the
space of combinatoric rewritings of machine instructions that will yield
the same effects. That is, find some other sequence of instructions that
hs the same net effect, but is shorter or runs faster or uses less
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

But we're geting ahead of ourselves. The above can come only after some
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
correspondance.

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
A fourth sense occurs as a meta-problem: in the sapce of all possible
sudoku-solving algorithms, how do we find the one that generates the
best possible paths? Or, at least, generates reasonably good ones on a
statistical ensemble of all possible sudoku puzzles?

This fourth item in the paragraph above becomes, again, as
structure-discernment problem, reified. What is the space of
sudoku-solving algorithms, and how can that space be described,
represented, characterized and operated upon? Obviously, there is an
infinite regress here; however, at each step of the game, there is a
fairly urgent desire to have optimal representations and optimal
algorithms for working with them.

Thus, a part (a large part?) of the task here is to obtain, create and
work with resonable algorithms that can do optimal things, without
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
queriable, and, better yet, optimally queriable. It is no accident that
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
exploring them entiely).

But how does one know if a branch is finite or not? There might be a
small and narrow and very thin tube that connects it to some other vast
network. Like two giant cave systems connected by a small passage, you
could wander around in one cave system for a very long time, failing to
discover the path to the other.

Well, so in this very round-about way, we arrive at the idea of using
LLM's to search for and discover optimal paths and structures and
algorithms.


### Intrefacing to LLM's
Dynamic prompts

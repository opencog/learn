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
preliminary structure has been discerned.


### Intrefacing to LLM's
Dynamic prompts

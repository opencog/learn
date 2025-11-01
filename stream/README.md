Dynamic text streaming
----------------------
The text processing framework used by this project, from it's inception
to about 2022, was a batch-processing framework. This proved unworkable,
as it was difficult to manage processing stages, to control the size of
the datasets, and frankly, baby-sitting the batch processing felt like
doing laundry: tedious and boring, consisting of mostly waiting.
(The batch-processing code is in the various "run" directories.)

The AtomSpace now has a nascent streaming infrastructure, together with
some prototype sensorimotor code, and an attempt to provide more agentic
interfaces into the text processing stream. The code in this directory
is (will be) an attempt to provide a more dynamic interface into text.
Something that could work with unix files and social media. Whatever.

The proximal driver of the code here is a project that attempts to
supplement Claude (or perhaps any LLM) with off-line long-term memory,
analysis and reasoning, a "meta-cognitive layer", external to the
servers that the LLM's run on. Is this a project worth doing? I dunno.
It seems interesting at the moment.

The interface to the LLM is "tokens", or more precisely "text", or
whatever the Anthropic LLM client can be coaxed to print on screen and
perform via MCP interfaces. So this is a bit of a narrow straw, backed
by a big brain at the other end of it. What can be done at this end of
the straw? Well, you guessed it. I cannot out-LLM the LLM guys; its a
multi-billion-dollar industry. But I can explore trying to find a
"neuro-symbolic representation", with sparse weights and graph structures
stored in, you guessed it, the AtomSpace. Duhh. Store what? Store How?

So in my idle daydream, I envision an extremely high-dimensional vector
space, but the basis elements of the vectors are, QueryLinks and
FilterLinks and DualLinks, and of course EgdeLinks as a primary graph
representation. But how do build this? I can't say "Claude build this",
that doesn't work.

But if I ask Claude to "build me something" it will happily propose some
stunningly naive approach from some 1980's AI textbook.  For a machine
trained on hundreds of thousands of the latest academic journal articles,
this is deeply disappointing. But we role with the punches.

Claude proposes semantic extraction into graph structures, so OK, this
is very old-school, maybe 1990's AI now. The big difference now is that
perhaps I can do this at scale, instead of having to hire 100 grad
students to do it, the way Dough Lenat tried to build Cyc. So loosely
speaking, a broken analogy, I'll let Claude try to recreate Cyc, but
now embedding the graphical relationships into multiple (two? five?
one hundred?) high-dimensional vector spaces encoding frequentist,
or Bayesian, or Markov, or Gibbs-entropic whatever weights in it. Put
Claude in the drivers seat, and I'll be the navigator.

Well, of course, Claude is lost right out the gate, and needs to be kept
on a very very short leash. It's attention span is just abysmal, and it
is almost impossible to get it to RTFM. So .. we are processing text.
Claude has created about 100 itty bitty utilities that do this and that,
its all disorganized, its all spaghetti code, can Claude can't remember
what it created last week, even though it is right there, in the same
directory. And this is why I want to create a memory system for it: so
it can remember these things. I can force it to RTFM once, but after
that, there needs to be a little birdie reminding him to take another
look before making a decision. I want that little birdie to be written
in Atomese, to run in the AtomSpace.

Why Atomese? Well, the pastiche of 100 scriptlets will never work, so
I have to supervise Claude and get it to write well-architected code.
Now, others would chose python for this task. I chose Atomese because
Atomese can be introspected. Of course, python can be introspected too:
just ask Claude to read the text file. But Atomese is different: you
don't "read a text file" and "try to understand it" by trying to 
"imagine what it does in your head". Code held graphical form is
already in graphically structured form. You can just run QueryLink
over it. If you want to overlay it with equivalent pseudocode, you
can also do that; its all in the AtomSpace, as graphs.

This directory contains (will contain) quasi-architected code, written
mostly by Claude, with as much supervision as I have patience for, with
the hope that it can supplant the earlier batch-processing code, while
remaining true to the idea of exploring neuro-symbolic learning.


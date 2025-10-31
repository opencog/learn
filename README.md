Structure Learning
==================
This project is a broad-based exploration of structural learning.
It has been fashionable to call this "neuro-symbolic learning".
An earlier incarnation of this project called it "Natural Language
Learning."  The vision and goals have broadened: the goal is to
learn any kind of structure that can appear in nature, in the noosphere.
The relationships between things, in full abstraction.

This is possible in part because the first version, confined to natural
language, was more-or-less a success. Not a roaring success like deep
learning; there was no chatbot that held the sum-knowledge of all things
that you could converse with. But it did show that you could use a
frequentist approach of "counting things" to obtain vector representations
that were comparable to early NN models, such as Word2Vec or Glove.
Comparable in what way? Well, all the party tricks you could play with
those could be done here as well. The representation here, though, is
radically different: it is graphs. Symbolic graphs. Hypergraphs.
Structures that have explicit interpretations as symbols and relations.
That can be crawled over. That can be queried, using an explicit query
language. And yet, at the same time, it can also be understood as a
collection of very high-dimensional vector spaces (more than one!)
layered on top of these graphs. Or intertwined with these graphs; take
your pick.

Where we were, where we are, the technology
-------------------------------------------
The Version One code is in the [version-one](version-one) directory.
Its mostly frozen in amber, now. The multiple README files there talk
about language learning and structure learning in a very explicit
fashion. However, rathe than migrating them to fit the times, it seems
easier to start over; so here we are: Version Two.

The underlying technologies are these. The representational system is
called [Atomese](https://wiki.opencog.org/w/Atomese). This is a
hypergraph representation system where both the symbols and relations
as well as the query language, are on equal footing: they're all
(hyper-)graphs. These graphs are not just passive representations,
but are also active: most graphs can be executed, and perform some
action when executed. Thus, it vaguely resembles a programming
language. But, unlike a programming language, it is also a graph.
Imagine, if you wish, a function with inputs and outputs, but now
we have those inputs and outputs connected with graph edges, going
to wherever they are connected. So, a large collection of
[Abstract Syntax Trees](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
Not just low-level graphs representing `(Plus 2 2)` but also high
level graphs, like `(Rewrite (A (Variable X) B) (D B A (Variable X)))`
Think, perhaps regex-as-a-graph, or perhaps SQL-as-a-graph, but far
more powerful/sophisticated than SQL. But also, other graphs are not
executable: words and sentences, for example. Yu can attach vectors
to words and sentences, You can also attach vectors to AST's and
to rewrite rules and to queries... its a generic symbolic representation
system that is interlayered with vectors and weights.

Version one had multiple serious problems. The worst was perhaps that
it was a batch processing model. A text corpus is downloaded from
somewhere (Project Gutenberg, Wikipedia), shoved through a pipeline,
sliced and diced, with the [AtomSpace](https://github.com/opencog/atomspace)
accumulating the results. The experience of running this was like
doing laundry: lots of work interleaved with lots of waiting and the
occasional emergency when you put in too much soap and the washing
machine is spilling out onto the floor.  Batch processing is not fun.

### Sensorimotor systems and agency
This prompted exploration of a more autonomuous design; the processing
would be done by agents, each having considerable independence and
decision-making authority. This required a sensori-motor subsystem,
so that the agents could grab and manipualte data, work with it, move
and read and copy. The current alpha (version-0.5) sensorimotor
Atomese is located in the [sensory](https://github.com/opencog/sensory)
git repo.

### Stream processing
Together with basic work on a sensori-motor system is work on a
stream processing system. Prototypical parts are the
[FilterLink](https://wiki.opencog.org/w/FilterLink) and the
[RuleLink](https://wiki.opencog.org/w/RuleLink).
There are an assortment of example demos that can be found in the
[Atomespace examples directory](https://github.com/opencog/atomspace/tree/master/examples/atomspace)
and also, for example, the
[parse-pipeline](https://github.com/opencog/sensory/blob/master/examples/parse-pipeline.scm)
demo in the sensory project. These demos are alarmingly complicated!!
No joke, this is not for the faint of heart.  A few words on stream
processing are due. Of course, this is an ancient idea: Apache Flink,
Apache Storm, RabbitMQ Streams, and on and on. These are popular and
production-ready. The Atomese streams are the exact opposite of that.
Why? The Atomese streams are an attempt to create a graph programming
system for data processing. Why? you might ask. Why not just use Java
or Python? Answer: Java and Python aren't graphs. You can't run a query
on them, except perhaps with `grep` and `find` and `sed` and `awk`.
Poor-mans tools. You certainly can't slap a 1024-dimmensiona
floating-point weight vector onto the 42nd line of `foobar.py`. You
can ask your LLM to do that, but its not the same thing. Thus,
Atomese streams are written in ... Atomese. For these reasons.
Its a bit brutish and nasty, at the moment, but this is a science
research project, not a software engineering project.

### Version Two
Work on Version Two has barely begun: this README, and the
[README in the stream directory](stream/README.md) is almost the only
public content.

I (now speaking personally, "I" am Linas Vepstas) anticipate that most
of the Version Two code will be written by Anthropic Claude. I will keep
it on a short leash, but I expect some fair amount of LLM slop. I'll try
to control it, as best as my patience allows. There will be scaffolding,
cruft and broken-ness interwoven with a reasonable architecture and
fairly clear design goals.

The proximal motivation for this is an attempt to endow Claude, or any
LLM, with "offline" memory (long-term memory) and reasoning skills
that operate outside of the LLM itself, acting to direct and stear and
enhance the capabilities of the LLM, acting as some sort of meta-layer,
some harness or reins or bridle, or perhaps a prefrontal cortex. Pick
your analogy. The project is perhaps foolish, or perhaps interesting;
at any rate, it is an attempt to interface a rather sophiticated LLM
to a rather meagre symbolic system. Is it doomed to fail? Well, sraight
out of the gate, its clear that Claude's ADHD and utter inability to
remember anything unless it is jammed into some prompt, and even then,
its utter unwillingness to RTFM whether written by itself, or anyone else,
well this is a fine challenge.  I'm up for fine challenges.

More will be posted when there's more to post.

-------
The End.

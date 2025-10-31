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
langugage. But, unlike a programming langauge, it is also a graph.
Imagine, if you wish, a function with inputs and outputs, but now
we have those inputs and outputs conneted with graph edges, going
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


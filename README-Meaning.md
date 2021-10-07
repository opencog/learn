
Extracting Meaning
==================
This text sketches an algorithm for extracting meaning from natural
language text. The intended sense of "meaning" here is the conventional
one: what you might find in a dictionary or encyclopaedia, albeit
in machine-readable format. A more precise defintion will become clear
as the algorithm is sketched out.

It seems to be time to start this: there's been fair success with
learning grammar, and that code seems to be debugged enough and genric
enough that it should be able to support the next level, namely, this
project.

The rough idea is to apply the same algorithm as before: of finding
pairs, and then creating disjuncts, except this time, the pairs will be
pairs of Sections, instead of Words. I beleive that such pairs of
Sections will be able to capture idioms, instititional phrases, and the
like with ease. I beleive the corresponding MetaSections will be able to
capture conceptual information, such as "intensional" properties of
objects (e.g. animals are furry, have a tail, have for feet, can run,
have babies, and need to eat.) I beleive MetaSections will also be
capable of performing anaphora resolution.

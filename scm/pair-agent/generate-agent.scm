;
; generate-agent.scm
; Generate text
;
; link-generator -c 4 -k 4 -l en 
; link-generator -l en -s 0
; This is * test
;
; General issues:
; * Allow link-parser app to special case wild-cards for generation.
; * Loading of entire dict is unfeasible, so...
;
; Ideas:
; -- Parse given word seq containing blanks, use unknown word for
;     blanks, then lookup all words with matching disjuncts. This
;     requires:
; * Allow dict lookup of words according to desired disjuncts.
;
;
OK, screw that. The diary part ten is exploring this, here are the best
ideas in summary form:

1) Starting with a word A, use incoming set to randonly select some
subset of all possible edges. This will be called the "focus set" or the
"attentional focus". Because it is small, it can be exhaustively
iterated and sorted into high-to-low MI order. This provides a weighted
distribution, from which a rescaled uniform-random choice can be made.

2) Random sample N out of full incomng set.



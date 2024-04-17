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



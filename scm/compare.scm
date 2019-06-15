;
; compare.scm -- Compare parses from two different LG dictionaries.
;
; -----------------------------------------------------------------
; Overview:
; ---------
; Goal is to obtain accuracy, recall vs. LG.
;
(use-modules (opencog) (opencog exec) (opencog nlp))
(use-modules (opencog nlp lg-dict) (opencog nlp lg-parse))


(LgDictNode "en")

(cog-execute!
(LgParseLink
	(PhraseNode "this is a test.")
	(LgDictNode "en")
	(NumberNode 1)))

;
; mst-parser.scm
;
; Wrappers for Maximum Spanning Tree and Maximum Planar Graph parsers.
;
; DEPRECATED/OBSOLETE. This is in the process of being replaced by
; code that does MST/MPG parsing using the LG framework. The goal is
; a more integrated approach.
;
; Copyright (c) 2014, 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; The scripts below are utility wrappers for the Maximum Spanning-Tree
; (MST) and the Maximum Planar Graph parsers, adapting the core parsers
; to format suitable for natural language.  That is, they're wrapped so
; that they can parse text sentences.  The parse graphs are then used
; to create a set of Sections describing the graph that was found,
; locally (lexically).
;
; Input to these are expected to be a single unicode utf8-encoded text
; sentence.  It is presumed, as background, that the atomspace is loaded
; with a large number of word-pairs and their associated mutual
; information.  These word-pairs need to have been previously computed.
;
; The sentence is tokenized, assuming that white-space represents word
; boundaries. Leading and trailing punctuation is stripped from each
; word, and is treated as a distinct "word".
;
; The set of words is treated as the set of vertices of a complete graph
; or "clique", with the edges being word-pairs. The MST parser obtains
; the spanning tree that maximizes the sum of the mutual information (or
; other additive quantity) associated with the edges. This tree is the
; MST tree.  The MPG parser does the same, but then adds edges, one at
; a time, having the next-highest MI value, until the largest possible
; planar graph is obtained (or no such edges (word-pairs) exist in the
; AtomSpace.)
;
; After a sentence has been analyzed by either parser, the resulting
; graph (i.e. edges between words) can be broken up into individual,
; local Sections. Here, a Section (also sometimes called a "pseudo-
; disjunct") is a single word (of the sentence), called a "germ" or
; "root", and a list of connectors indicating the connections
; (half-edges) made to the other words. This germ+connector-list
; combination thus provides a local, lexical description of the parse
; graph.
;
; The description is "lexical" in the sense that knowing the germ or
; root is enough to find the assocciated connector list (and then
; reassemble the original graph from these pieces).
;
; Functions included in this file:
; * A tokenizer, which splits a text sentence into a list of strings.
; * Wrappers for the MST and MPG parsers, which take text strings,
;   tokenize them, and pass them through the parsers proper.
; * A utility to print the resulting graph.
; * A second wrapper that combines the parser with the step that
;   extracts sections (disjuncts).
;
; ---------------------------------------------------------------------
;
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-11))
(use-modules (opencog matrix))
(use-modules (opencog sheaf))

; ---------------------------------------------------------------------
;
(define-public (tokenize-text plain-text)
"
  tokenize-text plain-text -- split sentence into words.

  Tokenize the text: take the input sentence (as a UTF-8 encoded string),
  and return a list of the words in the sentence (as a list of strings).
  It is assumed that words are always separated by white-space, so this
  is easy. The tokenizer also makes a vague attempt to also separate
  punctuation, although it is not terribly robust in doing so. It does
  make a limited attempt to split words with certain infixed
  punctuation, such as double-dashes and long dashes.

  This is not terribly rigorous; it treats a somewhat arbitrary
  selection of oddball unicode punctuation marks as prefixes and
  suffixes. This list is not complete nor well-organized; rather,
  it is built up from experience of parsing assorted texts and noting
  the kinds of stuff that actually gets used. Its slanted towards
  European languages, and may be inadequate for other languages.

  I did not want to get too fancy here; I want just enough to parse
  most ordinary text, for now.  A fancier treatment must await
  generalized handling of morphology, at which point, we can treat
  any kind of affixes, and not just punctuation.  So its kind of
  pointless to try to replace the code below by something better,
  unless the better thing is full morphology support.

  See also: `split-text`, which tokenizes only according to whitespace,
     and completely ignores punctuation.
"
	; Prefix and suffix lists taken from the link-grammar ANY
	; language 4.0.affix file.
	(define prefix "({[<«〈（〔《【［『「``„“‘'''\"…..._-‐‑‒–—―¿¡$£₤€¤₳฿₡₢₠₫৳ƒ₣₲₴₭₺ℳ₥₦₧₱₰₹₨₪﷼₸₮₩¥៛호점†‡§¶©®℗№#")
	(define suffix ")}]>»〉）〕》】］』」’'\"%,.。:;?!‽؟？！…”_-‐‑‒–—―、～¢₵™℠")

	; Hey, the long-dashes below all look alike, but are actually
	; different. Same for the short dashes. The first dash is the
	; ascii-dash 0x2d. The rest are UTF-8 multi-byte encodings.
	(define infix "-‐‑‒–—―…()[]{}")
	(define prefix-list (string->list prefix))
	(define suffix-list (string->list suffix))
	(define infix-list (string->list infix))

	; Tail-recursive helper function. Strip off any prefixes appearing
	; in the list prefli, and return a list of the stripped prefixes,
	; and the remaining word.
	(define (strip-prefli word prefli)
		(if (null? prefli)
			(list word)  ; prefix list is empty, return the word.
			(let* ((punct (car prefli))
					(head (string-ref word 0)))
				(if (eq? punct head) ; it there's a match, then split
					(append
						(list (string punct))
						(strip-prefix (substring word 1)) ; loop again.
					)
					(strip-prefli word (cdr prefli)) ; if no match, recurse.
				))))

	; Main entry point for the recursive prefix splitter
	(define (strip-prefix word)
		(if (< 0 (string-length word))
			(strip-prefli word prefix-list)
			'()))

	; Tail-recursive helper function. Strip off any suffixes appearing
	; in the list sufli, and return a list of the stripped prefixes,
	; the remaining word, and the stripped suffixes.
	(define (strip-sufli word sufli)
		(if (null? sufli)
			(strip-prefix word)
			(let* ((punct (car sufli))
					(len (- (string-length word) 1))
					(tail (string-ref word len)))
				(if (eq? punct tail)
					(append
						(strip-affix (substring word 0 len))
						(list (string punct))
					)
					(strip-sufli word (cdr sufli))
				))))

	; Main entry point for the recursive splitter
	(define (strip-affix word)
		(if (< 0 (string-length word))
			(strip-sufli word suffix-list)
			'()))

	; Pad dashes with whitespace.
	; Taking string str, starting at the start-index, if it
	; contains the infx character, then pad it with a space.
	(define (pad-a-dash str infx start)
		(define idx (string-index str infx start))
		(if idx
			(let ((idp1 (+ idx 1)))
				(pad-a-dash
					(string-replace str " " idx idx)
					infx
					(+ idx 2)))
			str))

	; Pad dashes with whitespace.  Go over every character in the
	; infix-list and if it occurs in the string, put a blank space
	; in front of it.  The string splitter will then split at the
	; blank space, and the prefix stripper will do the rest.
	(define (pad-dash str ifx-list)
		(if (null? ifx-list) str
			(pad-dash (pad-a-dash str (car ifx-list) 0) (cdr ifx-list))))

	; Merge certain types of punctuation back into one.
	; e.g. three dots, or two dashes.
	(define (remerge tkl buff punct rslt)
		(if (null? tkl)
			(if (< 0 (string-length buff)) (cons buff rslt) rslt)
			(if (string=? (car tkl) punct)
				(remerge (cdr tkl) (string-append buff punct) punct rslt)
				(if (< 0 (string-length buff))
					(remerge tkl "" punct (cons buff rslt))
					(remerge (cdr tkl) "" punct (cons (car tkl) rslt))))))

	; Merge a sequence of dots back into one word.
	; Merge a sequence of ascii dashes back into one word.
	(define (remerge-dot-dash tkl)
		(remerge (remerge tkl "" "." '()) "" "-" '()))

	; The left-wall indicates the start of the sentence, and
	; is used to link to the head-verb, head-noun of the sentence.
	(define left-wall "###LEFT-WALL###")

	(let* ((pad-text (pad-dash plain-text infix-list))
			(word-list (string-split pad-text #\ ))
			(strip-list (map strip-affix word-list))
			(tok-list (concatenate (cons (list left-wall) strip-list)))
			(merge-list (remerge-dot-dash tok-list))
		)
		; (format #t "strip-list is ~A\n" strip-list)
		; (format #t "tok-list is ~A\n" tok-list)
		; (format #t "merge-list is ~A\n" merge-list)
		merge-list
	)
)

; ---------------------------------------------------------------------
;
(define (parse-setup-tool parser plain-text)
"
  Handy dandy utility to avoid excess cut-n-paste for
  customization.
"
	; Tokenize the sentence into a list of words.
	(define word-strs (tokenize-text plain-text))

	; Create a sequence of atoms from the sequence of strings.
	(define word-list (map WordNode word-strs))

	; Define where the costs are coming from.
	; We'll be using the MI scores coming from the random planar trees.
	(define pair-obj (make-any-link-api))
	; (define pair-obj (make-clique-pair-api))

	(define mi-source (add-pair-freq-api pair-obj))

	(define scorer (make-score-fn mi-source 'pair-fmi))

	; Assign a bad cost to links that are too long --
	; longer than 16. This is a sharp cutoff.
	; This causes parser to run at O(N^3) for LEN < 16 and
	; a faster rate, O(N^2.3) for 16<LEN. This should help.
	; For edges linking words more than 8 apart, progressively
	; ramp down the score; we don't want long links in general.
	(define (ramp-scorer LW RW LEN)
		(define MAXLEN 16)
		(define RAMPLEN 8)
		(define mplu1 (+ MAXLEN 1))
		(define (ramp len) (/ (- mplu1 len) (- mplu1 RAMPLEN)))

		(if (< MAXLEN LEN) -2e25
			(let ((sco (scorer LW RW LEN)))
				; Negative scores pass right through. Others get ramped.
				(if (or (< LEN RAMPLEN) (< sco 0))
					sco
					(* sco (ramp LEN))))))

	; Process the list of words.
	(parser word-list ramp-scorer)
)

; ---------------------------------------------------------------------
;
(define-public (mst-parse-text plain-text)
"
  mst-parse-text -- Maximum Spanning Tree parser.

  Given a raw-text sentence, it splits apart the sentence into distinct
  words, and finds an (unlabelled) dependency parse of the sentence, by
  finding a dependency tree that maximizes the mutual information.
  Returns a list of word-pairs, together with the associated mutual
  information.
"
	(parse-setup-tool mst-parse-atom-seq plain-text)
)

; ---------------------------------------------------------------------
;
(define-public (mpg-parse-text plain-text)
"
  mpg-parse-text -- Maximum Planar Graph parser.

  Given a raw-text sentence, it splits apart the sentence into distinct
  words, and finds an (unlabelled) dependency parse of the sentence, by
  finding a dependency graph that maximizes the mutual information,
  and maximizes the number of edges while keeping the graph planar.
  Returns a list of word-pairs, together with the associated mutual
  information.
"
	(define (mpg-linear ATOM-LIST SCORE-FN)
		; Number the atoms in sequence-order.
		(define numa-list (atom-list->numa-list ATOM-LIST))

		; Start with the MST parse
		(define mst-tree (graph-add-mst '() numa-list SCORE-FN -1))

		; Add the mpg edges
		(define mpgraph (graph-add-mpg mst-tree numa-list SCORE-FN -1))

		; Connect up disconnected words
		(define disco (graph-add-linear mpgraph numa-list))

		; Connect up islands
		; NOOOO! This last step goes exponentially as the size of the
		; graph. Its 2 seconds for 50-word sentences; it's 30 seconds
		; for 90-word sentences and its 300 for 100-word sentences!
		; Yes, long sentences are fundamentally broken (e.g. tables
		; of contents, indexes, price-lists, etc.) but still...
		; Are bridges even important? Or is this a feel-good attempt?
		; Screw it, decide some other day. For right now, just clamp.
		(if (< 30 (length numa-list))
			disco
			(graph-add-bridges disco))
	)

	(parse-setup-tool mpg-linear plain-text)
)

; ---------------------------------------------------------------------
; Return #t if the section is bigger than what the current postgres
; backend can store. Currently, the limit is atoms with at most 330
; atoms in the outgoing set.
;
; This typically occurs when the MST parser is fed a long string of
; punctuation, or a table of some kind, or other strings that are not
; actual sentences.
(define (is-oversize? SECTION)
	(< 330 (cog-arity (gdr SECTION)))
)

(define-public (observe-mst plain-text)
"
  observe-mst -- update pseduo-disjunct counts by observing raw text.

  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up. Uses the MST parser to obtain
  a spanning tree parse.
"
	; The count-one-atom function fetches from the SQL database,
	; increments the count by one, and stores the result back
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections (mst-parse-text plain-text))
	)
)

(define-public (observe-mpg plain-text)
"
  observe-mpg -- update pseduo-disjunct counts by observing raw text.

  This is the second part of the learning algo: simply count how
  often pseudo-disjuncts show up. Uses the MPG parser to obtain
  the maximal planar graph.
"
	; The count-one-atom function fetches from the SQL database,
	; increments the count by one, and stores the result back
	(for-each
		(lambda (dj) (if (not (is-oversize? dj)) (count-one-atom dj)))
		(make-sections (mpg-parse-text plain-text))
	)
)

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
;
; (use-modules (opencog) (opencog persist) (opencog persist-sql))
; (use-modules (opencog nlp) (opencog learn))
; (sql-open "postgres:///en_pairs?user=linas")
;
; (load-atoms-of-type 'WordNode)
; (fetch-any-pairs)
; (mst-parse-text "faire un test")
; (mst-parse-text "Elle jouit notamment du monopole de la restoration ferroviaire")
;
; (define my-word-list (atom-list->numa-list
;      (tokenize-text "Elle jouit notamment du monopole de la restoration ferroviaire")))
; answer: du monopole 5.786411762237549
;      (tokenize-text "faire un test entre quelques mots")))
; answer: faire quelques 12.62707805633545
;      (tokenize-text "grande petit mot liste pour tout occasion")))
;
; (define my-start-pair (pick-best-cost-pair lg_any my-word-list))

;
; jaccard.scm
;
; Given a pair of words, determine club membership based on common
; traits (bassed on the jaccard distance between all members of the
; group).
;
; Copyright (c) 2021,2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file implements the selection of individuals into a group (or
; "cluster" or "class" or "club" - all synonyms.)  The individuals are
; NOT merged together; this is done elsewhere. Only the selection is
; done here.  The selection is done by maximizing the Jaccard distance
; between all members of the group.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; Where the simiarity scores will be stored
(define SIM-ID "shape-mi")

; ---------------------------------------------------------------

(define-public (make-jaccard-selector LLOBJ
	QUORUM COMMONALITY NOISE)
"
  make-jaccard-selector LLOBJ QUORUM COMMONALITY NOISE

  Return a function that selects the members of a group, by maximizing
  the Jaccard similarity between all members of the group.

  Example usage:

     (define select-group
         (make-jaccard-selector covr-obj 0.7 0.2 3))
     (select-group WA WB WLIST)

  The select-group function accepts two individuals WA and WB that will
  be the founding members of the group, plus a list WLIST of all
  individuals that have similarity scores precomputed. The selection
  process is as follows:

  An initial group of candidate members, the `in-group`, is constructed
  by using MI similarity to each of the two founding members WA, WB.
  This in-group is selected by `optimal-in-group`, implemented
  elsewhere.

  Next, the fraction of disjuncts that all group members have in common
  is computed. If that shared fraction is greater than COMMONALITY, then
  the selection process is done. Otherwise, a group member is ejected,
  and the fraction is recomputed. If it is better, it is accepted; the
  process is repeated until the either the fraction exceeds COMMONALITY
  or the highest possible fraction has been found.

  There are two ways of ejecting candidates: one is to remove the one
  at the tail of the initial list. The other way is to loop over all
  of the members, testing the ejection of each in turn. The second
  variant is hard-coded. The first variant is stubbed out in the code.
"
	; The ordinary MI similarity of two words
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The similarity function to use for the in-group formation.
	; Hypothesis: ordinary MI creates better clusters.
	; (define IN-GRP-SIM ranked-mi-sim)
	(define IN-GRP-SIM mi-sim)

	; Log what we actually used.
	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(cog-set-value! *-log-anchor-* (Predicate "in-group-sim")
		(StringValue "mi-sim"))

	; ------------------------------
	; Tail-recursive trimmer; rejects large groups with little
	; commonality. Accepts first grouping with commonality above
	; the threshold COMMONALITY, or the last grouping before the
	; commonality decreases.
	;
	; Tail-recursive means that only candidates from the end of
	; the list are trimmed away.  See the exhaustive-search version,
	; below, for an alternate approach.
	(define (trim-tail-rec GRP prev-com prev-grp)
		(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
		(define comality (/ (car ovlp) (cadr ovlp)))
		(format #t "Club size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
			(length GRP) (car ovlp) (cadr ovlp) (* comality 100))

		; In plain English:
		; If comality is above threshold, accept.
		; If comality dropped, compared to the previous,
		;    accept the previous.
		; If we are down to two, accept. Do this check last.
		; Else trim one word from the end, and try again.
		(cond
			((< COMMONALITY comality) GRP)
			((< comality prev-com) prev-grp)
			((= (length GRP) 2) GRP)
			(else (trim-tail-rec (drop-right GRP 1) comality GRP))))

	(define (trim-tail GRP)
		(trim-tail-rec GRP -1.0 GRP))

	; ------------------------------
	; mask out the IDX'th enty in the LST. That is, return a
	; list with the IDX'th entry removed.
	(define (mask LST IDX)
		(append (take LST IDX) (drop LST (+ IDX 1))))

	; Greedy hill-climbing trimmer. Explores dropping members of
	; the group, to see if the commonality improves. If so, it tries
	; again, else it halts. This assumes a simple landscape; so,
	; yes, in principle, it can get trapped in local maxima, but
	; it seems likely that there will always be some direct path
	; to the top.
	;
	; Halts early and accepts first grouping with commonality above
	; the threshold COMMONALITY.
	(define (trim-greedy-rec GRP)

		(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
		(define cmlty (/ (first ovlp) (second ovlp)))
		(when (< comality cmlty)
			(set! comality cmlty)
			(set! best (append ovlp GRP))

			; Now, recurse and try to see if we can do even better.
			; If the group has two members, it cannot be shrunk any more.
			; If the group already exceeds the commonality bound, we are
			; done. Else, drop one, and see what happens.
			(define glen (length GRP))
			(when (and (< 2 glen) (< comality COMMONALITY))

				; Loop depth-first until we find something that exceeds
				; COMMONALITY.
				(any
					(lambda (N)
						(define rslt (trim-greedy-rec (mask GRP N)))
						(define cmlty (/ (first rslt) (second rslt)))

						; If its better than what we have, record it.
						(when (< comality cmlty)
							(set! comality cmlty)
							(set! best rslt))

						; If its better than the threshold, we are done.
						(< COMMONALITY comality))
					(iota glen 0)))

			; Print a progress report.
			(format #t "Best so far size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
				(- (length best) 2) (first best) (second best) comality))

		; Return the best result so far.
		best)

	; Wrapper for above.
	(define (trim-greedy GRP)
		(define comality 0)
		(define best (append (list 0 1) GRP))

		; Start by reversing the list, so that the seed members are
		; explored last.
		(trim-greedy-rec (reverse GRP))

		(format #t "Best size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
			(- (length best) 2) (first best) (second best) comality)

		; Drop the leading overlap numbers before returning.
		(drop best 2))

	; ------------------------------
	; Find the largest in-group that also shares more than a
	; fraction COMMONALITY of disjuncts among a QUORUM of members.
	; The returned group will always have at least two members,
	; the initial two proposed.
	(define (max-jaccard-grp WA WB WLIST)

		; Chop down the WLIST to a more manageable size.
		(define initial-in-grp
			(optimal-in-group IN-GRP-SIM WA WB WLIST))

		(format #t "Initial in-group size=~D:" (length initial-in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
			initial-in-grp)
		(format #t "\n")

		; Remove members from the group, until either COMMONALITY
		; is exceeded, or a maximum is hit.
		; (trim-tail initial-in-grp)
		(trim-greedy initial-in-grp)
	)

	; Return the function defined above.
	max-jaccard-grp
)

; ---------------------------------------------------------------
#! ========
;
; Example usage

(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

; If this hasn't been done, then it needs to be!
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

(define sap (add-similarity-api sha #f "shape-mi"))
(define asm (add-symmetric-mi-compute sha))

==== !#

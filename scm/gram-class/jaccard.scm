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
     (define initial-in-grp
         (optimal-mi-in-group SIM-FUN WA WB WLIST))
     (select-group initial-in-grp)

  An initial group of candidate members, the `in-group`, is constructed
  by using MI similarity to each of the two founding members WA, WB.
  This in-group is selected by `optimal-in-group`, implemented
  elsewhere.

  The jaccard-slector begins with this candidate list, and trims it down
  so that it meets the jaccard similarity criteria. This works as
  follows:

  The fraction of disjuncts that all group members have in common
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
	; the group, to see if the commonality improves. If so, pick
	; the direction that gave the steepest (best) improvement, and
	; try again. This assumes a simple landscape, one where the
	; steepest ascent will lead to the top. This feels like a
	; reasonable assumption, mostly because the group is quite
	; small, usually less than ten members, and Jaccard overlaps
	; seem like they should give a simple landscape. At any rate,
	; greedy ascent is a lot faster than an exhaustive search,
	; which would take N-factorial steps for a group of size N.
	;
	; Halts early and accepts first grouping with commonality above
	; the threshold COMMONALITY.
	(define (trim-greedy-rec cmlty ovlp GRP)

		; If the group has two members, it cannot be shrunk any more.
		; If the group already exceeds the commonality bound, we are
		; done.
		(define glen (length GRP))
		(if (or (= 2 glen) (<= COMMONALITY cmlty))

			; We are done. Just return what we have.
			(append ovlp GRP)

			; Loop and try to see if we can do better.
			; This is a breadth-first loop.
			(let ((best-cmlty cmlty)
					(best-ovlp ovlp)
					(best-grp GRP))

				; Loop until we find something that exceeds COMMONALITY.
				(any
					(lambda (N)
						(define grp (mask GRP N))
						(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE grp))
						(define cmlty (/ (first ovlp) (second ovlp)))

						; If its equal or better than what we have, record it.
						; Note that both best-cmlty and cmlty might be zero,
						; so we want to continue searching, anyway.
						(when (<= best-cmlty cmlty)
							(set! best-cmlty cmlty)
							(set! best-ovlp ovlp)
							(set! best-grp grp))

						; If its better than the threshold, we are done.
						(< COMMONALITY cmlty))
					(iota glen 0))

					; If there was an improvement, try again.
					(if (not (equal? best-grp GRP))
						(begin
							; Print a progress report.
							(format #t "Better: size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
								(length best-grp)
								(first best-ovlp) (second best-ovlp)
								(* 100 best-cmlty))

							(trim-greedy-rec best-cmlty best-ovlp best-grp))
						(append ovlp GRP)))))


	; Wrapper for above.
	(define (trim-greedy GRP)

		(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
		(define cmlty (/ (first ovlp) (second ovlp)))

		(format #t "Start:  size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
			(length GRP) (first ovlp) (second ovlp) (* 100 cmlty))

		(define best (trim-greedy-rec cmlty ovlp GRP))
		(define comality (/ (first best) (second best)))

		(format #t "Best:   size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
			(- (length best) 2) (first best) (second best) (* 100 comality))

		; Drop the leading overlap numbers before returning.
		(drop best 2))

	; ------------------------------
	; Find the largest in-group that also shares more than a
	; fraction COMMONALITY of disjuncts among a QUORUM of members.
	; The returned group will always have at least two members,
	; the initial two proposed.
	(define (max-jaccard-grp initial-in-grp)

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

(define sap (add-gram-mi-sim-api sha))
(define asm (add-symmetric-mi-compute sha))

==== !#

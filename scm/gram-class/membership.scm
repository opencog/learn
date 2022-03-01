;
; membership.scm
;
; Given a pair of words, determine club membership.
;
; Copyright (c) 2021,2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file implements the selection of individuals into a group
; (or "cluster" or "class" or "club" - each of these terms are
; used synonymously.)  The individuals are NOT merged together;
; this is done elsewhere. Only the selection is done here.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; Where the simiarity scores will be stored
(define SIM-ID "shape-mi")

; ---------------------------------------------------------------

(define-public (make-membership-selector LLOBJ
	QUORUM COMMONALITY NOISE)
"
  make-membership-selector LLOBJ QUORUM COMMONALITY NOISE
"

	; The ordinary MI similarity of two words
	(define sap (add-similarity-api LLOBJ #f SIM-ID))
	(define (mi-sim WA WB)
		(define miv (sap 'pair-count WA WB))
		(if miv (cog-value-ref miv 0) -inf.0))

	; The ranked MI similarity of two words
	;(define (ranked-mi-sim WA WB)
	;	(define miv (sap 'pair-count WA WB))
	;	(if miv (cog-value-ref miv 1) -inf.0))

	; The similarity function to use for the in-group formation.
	; Hypothesis: ordinary MI creates better clusters.
	; (define IN-GRP-SIM ranked-mi-sim)
	(define IN-GRP-SIM mi-sim)

	; Log what we actually used.
	(define *-log-anchor-* (LLOBJ 'wild-wild))
	(cog-set-value! *-log-anchor-* (Predicate "in-group-sim")
		(StringValue "mi-sim"))

	(cog-set-value! *-log-anchor-* (Predicate "quorum-comm-noise")
		(FloatValue QUORUM COMMONALITY NOISE NRANK))

	; ------------------------------
	; ------------------------------
	; Find the largest in-group that also shares more than a
	; fraction COMMONALITY of disjuncts among a QUORUM of members.
	; The returned group will always have at least two members,
	; the initial two proposed.
	(define (get-merg-grp WA WB CANDIDATES)
		(define initial-in-grp
			(optimal-in-group IN-GRP-SIM WA WB CANDIDATES))

		(format #t "Initial in-group size=~D:" (length initial-in-grp))
		(for-each (lambda (WRD) (format #t " `~A`" (cog-name WRD)))
			initial-in-grp)
		(format #t "\n")

		; Tail-recursive trimmer; rejects large groups with little
		; commonality. Accepts first grouping with commonality above
		; the threshold COMMONALITY, or the last grouping before the
		; commonality decreases.
		(define (trim-group GRP prev-com prev-grp)
			(define ovlp (count-shared-conseq LLOBJ QUORUM NOISE GRP))
			(define comality (/ (car ovlp) (cadr ovlp)))
			(format #t "In-group size=~D overlap = ~A of ~A disjuncts, commonality= ~4,2F%\n"
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
				(else (trim-group (drop-right GRP 1) comality GRP))))

		(trim-group initial-in-grp -1.0 initial-in-grp)
	)

	get-merg-grp
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

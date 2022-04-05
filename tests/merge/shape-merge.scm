;
; shape-merge.scm
; Unit test for merging of Shapes.
;
; This tests (unreliably) an order-dependent merge bug. It's unreliable,
; because the bug only shows up if the merge order is done wrong, and so
; this unit test won't always trigger on bad code. But .. hey, lets try.
;
; Here's the problem:
; Start with
;    N (f, a- a+)  and  M (f, b- a+)
; and merge a & b. The crosses are:
;    N [a, <f, $- a+>] + M [b, <f, $- a+>] -- Yes, merge!
;    N [a, <f, a- $+>] + 0 [b, <f, a- $+>] -- oh no!
;    M [a, <f, b- $+>] + 0 [b, <f, b- $+>] -- oh no!
; If we hit the first one, we merge and rebalance, and the merge
; knocks out the next two (since the merge rebuilds the sections
; and the cross-sections). But if either of the "oh no!"'s are
; hit first, the merge is rejected, and the "Yes, merge!" case
; is never considered, and so never happens. Hmm.  To fix this,
; the merge order on shapes is reordered so that the mergeable
; ones are always performed first.
;
; Created Jan 2022

(use-modules (opencog) (opencog matrix))
(use-modules (opencog nlp))
(use-modules (opencog learn))

(use-modules (opencog test-runner))
(use-modules (srfi srfi-64))

(opencog-test-runner)

(load "connector-setup.scm")
(load "shape-data.scm")

; ---------------------------------------------------------------
;
(define (run-shapes WA WB WC-NAME)

	; Load some data
	(setup-f-sections)

	; Define matrix API to the data
	(define pca (make-pseudo-cset-api))
	(set! gsc (add-covering-sections pca))

	; Verify that the data loaded correctly
	; We expect 2 sections on "f"
	(test-equal 2 (length (gsc 'right-stars (Word "f"))))

	; Get the total count on all Sections
	(define totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section))))

	; Create CrossSections and verify that they got created
	(gsc 'explode-sections)
	(test-equal 4 (length (cog-get-atoms 'CrossSection)))

	; Verify that direct-sum object is accessing shapes correctly
	; i.e. the 'explode should have created some CrossSections
	(test-equal 3 (length (gsc 'right-stars (Word "a"))))
	(test-equal 1 (length (gsc 'right-stars (Word "b"))))

	; We expect a total of 2 Sections
	(test-equal 2 (length (cog-get-atoms 'Section)))

	; --------------------------
	; Merge two sections together.
	(define frac 0)
	(merge gsc WA WB frac)
	(define WC-AB (WordClassNode WC-NAME))

	; We expect nothing remaining on a or b
	(test-equal 0 (length (gsc 'right-stars (Word "a"))))
	(test-equal 0 (length (gsc 'right-stars (Word "b"))))

	; We expect three merged sections
	(test-equal 2 (length (gsc 'right-stars WC-AB)))

	; Of the 2 original Sections, only one should remain
	(test-equal 1 (length (cog-get-atoms 'Section)))

	; Of the 4 original CrossSections, 2 should remain.
	(test-equal 2 (length (cog-get-atoms 'CrossSection)))

	; --------------
	; Validate counts.
	; For example:
	(define epsilon 1.0e-8)
	(define cnt-mrg (+ cnt-f-aa cnt-f-ba))
	(test-approximate cnt-mrg
		(cog-count (car (gsc 'right-stars (Word "f")))) epsilon)

	; To gain access to the counts, load them by name.
	(expected-a-b-sections WC-AB)
	(test-approximate cnt-mrg (cog-count sec-f-mm) epsilon)

	; Validate counts on select CrossSections...
	(test-approximate cnt-mrg (cog-count xes-m-f-vm) epsilon)
	(test-approximate cnt-mrg (cog-count xes-m-f-mv) epsilon)

	; -----------------------
	; Verify detailed balance
	(test-assert (check-sections gsc epsilon))
	(test-assert (check-crosses gsc epsilon))
	(test-assert (check-shapes gsc epsilon))

	; Verify no change in totals
	(test-approximate totcnt (fold + 0 (map cog-count (cog-get-atoms 'Section)))
		epsilon)
)

(define t-shape-merge "shape merge test")
(test-begin t-shape-merge)

	; Check both merge orders. Results should be independent of the order.
	(setup-database)
	(run-shapes (Word "a") (Word "b") "a b")

	(setup-database)
	(run-shapes (Word "b") (Word "a") "b a")
(test-end t-shape-merge)

; ---------------------------------------------------------------
(opencog-test-end)

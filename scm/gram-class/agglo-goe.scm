;
; agglo-goe.scm
;
; Loop over all words, merging them into grammatical categories.
; Agglomerative clustering, using GOE similarity to determine
; membership.
;
; Copyright (c) 2022 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; This file manages the top-most loop for traversing over all words,
; and assigning them to grammatical clusters using GOE similarity.
; This file does not provide tools for judging similarity, nor does
; it provide the low-level merge code.  It only manages the top loop.
;
; Under construction.
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public
(use-modules (opencog) (opencog matrix) (opencog persist))

; ---------------------------------------------------------------------
; ---------------------------------------------------------------------
#! ========
;
; Example usage.

; Load disjuncts into RAM, and create the matching shapes.
(define pca (make-pseudo-cset-api))
(define pcs (add-pair-stars pca))
(define sha (add-covering-sections pcs))
(sha 'fetch-pairs)
(sha 'explode-sections)

(fetch-atom (AnchorNode "data logger"))

; This should have been done much much earlier, and stored!
; It's needed for grammatical-MI similarity computations.
(define bat (batch-transpose sha))
(bat 'mmt-marginals)

; Also, grammatical-MI similarities should have been computed and
; stored! Fetch these!
(define smi (add-similarity-api pcs #f "shape-mi"))
(smi 'fetch-pairs)

; Here's where the GOE simillarities are stored.
(define gos (add-similarity-api smi #f "goe"))
(gos 'pair-count (Word "she") (Word "he"))
(gos 'get-count (Similarity (Word "she") (Word "he")))

==== !#

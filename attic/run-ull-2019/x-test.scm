;
; x-merge-test.scm
;
; Set up everything needed for the language-learning word-pair
; counting pipeline. Starts the REPL server, opens the database.
;
(use-modules (srfi srfi-1))
(use-modules (opencog) (opencog logger))
(use-modules (opencog persist) (opencog persist-sql))
(use-modules (opencog nlp) (opencog nlp learn))
(use-modules (opencog matrix) (opencog exec))
(use-modules (opencog sheaf))
(use-modules (opencog cogserver))

; Start the cogserver on port 19905
(start-cogserver "x-test.conf")

; Open the database.
; Edit the below, setting the database name, user and password.
; (sql-open "postgres:///en_pairs_rfive_mtwo?user=linas&password=asdf")
; (sql-open "postgres:///mrg_tst?user=linas&password=asdf")
(sql-open "postgres:///langtest?user=linas&password=asdf")

(define pca (make-pseudo-cset-api))
(define psa (add-pair-stars pca))

(define wpa (make-any-link-api))
(define wps (add-pair-stars wpa))

; (define psu (add-support-api pca))
; (define psc (add-support-compute psa))
; (define pfa (add-pair-freq-api psa))

; (define pta (add-transpose-api psa))
; (define pmi (add-symmetric-mi-compute pta))

; (define hva (make-shape-vec-api))
; (define hvs (add-pair-stars hva))
; (define hvp (add-support-api hvs))
; (define hst (make-store hva))
; (define bth (batch-transpose hva))

; --------
; (define cfa (add-pair-freq-api cvs))

(define gca (make-gram-class-api))
(define gcs (add-pair-stars gca))

;;; (psa 'fetch-pairs)
; (gcs 'fetch-pairs)

;;; (define asc (add-singleton-classes psa))
;;; (asc 'create-hi-count-singles 500)

;;; (define gcf (add-wordclass-filter gcs))
; (batch-all-pair-mi gcs)

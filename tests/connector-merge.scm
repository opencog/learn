;
; connector-merge.scm
;

(use-modules (opencog) (opencog martrix))
(use-modules (opencog nlp))
(use-modules (opencog nlp learn))
(use-modules (opencog persist) (opencog persist-rocks))

(use-modules (opencog test-runner))

(opencog-test-runner)

; ---------------------------------------------------------------
; Database management
(define (setup-database)
	(cog-atomspace-clear)
	(define storage-node (RocksStorageNode "rocks://tmp/test-merge"))
	(cog-open storage-node)
)

; ---------------------------------------------------------------
(define t-simple "simplest merge test")
(test-begin t-simple)

(load "connector-data.scm")

(setup-database)
(setup-basic-sections)
(define pca (make-pseudo-cset-api))
(define csc (add-covering-sections pca))
(csc 'explode-sections)
(define gsc (add-cluster-gram csc))



(define disc (make-discrim gsc 0.25 4 4))
(disc 'merge-function (Word "e") (Word "j"))


(test-equal "foo" #t (not #f))
(test-end t-simple)

; ---------------------------------------------------------------

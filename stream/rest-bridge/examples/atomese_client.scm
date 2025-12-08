;; atomese_client.scm
;;
;; Helper functions to be called from the REST bridge via CogServer.

(use-modules (opencog) (opencog exec))

;; Store one utterance in AtomSpace
(define (store-utterance txt)
  (let* ((node   (Concept txt))
         (uid    (Number (random 1000000000))))
    (cog-set-value! node (Predicate "utterance-id") uid)
    (cog-set-value! node (Predicate "raw-text") (StringValue txt))
    node))

;; Very simple "reasoning" placeholder:
;; Given text, check if we have a Concept with that name.
;; If yes, return a message; else, say it's new.
(define (reason-about-text txt)
  (let ((node (Concept txt)))
    (if (null? node)
        (string-append "NEW-UTTERANCE: " txt)
        (string-append "SEEN-BEFORE: " txt))))

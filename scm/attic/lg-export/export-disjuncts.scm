;
; export-disjuncts.scm
;
; Export disjuncts from the atomspace into a format suitable for
; use by the Link-Grammar parser.  This uses the sqlite3 format.
;
; Copyright (c) 2015 Rohit Shinde
; Copyright (c) 2017 Linas Vepstas
;
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; After a collection of disjuncts has been observed by the MST pipeline,
; and optionally clustered into grmmatical classes, they can be exported
; to the link Grammar parser, where they can be used to parse sentences.
;
; Needs the guile-dbi interfaces, in order to write the SQL files.
;;
;; XXX hack alert:
;; TODO WordClassNode support might be .. funky.
;; In particular, if a WordNode appears in a connector, it is replaced
;; by all WordClasses that it might be a part of. This is an
;; over-generalization, but needed for just right now.
;; This is done in the `connector-to-lg-cnr` function below.
;
; Example usage:
;     (define pca (make-pseudo-cset-api))
;     (define fca (add-subtotal-filter pca 50 50 10 #f))
;     (export-csets fca \"dict.db\" \"EN_us\")
;
; The subtotal filter rejects all words and connector-seqs that have
; been observed less than 50 times, and all individual entries that
; have been observed less than ten times.
;
; Then, in bash:
;    cp -pr /usr/local/share/link-grammar/demo-sql ./my-place
;    cp dict.db ./my-place
;    link-parser ./my-place
;
; ---------------------------------------------------------------------

(use-modules (srfi srfi-1))
(use-modules (ice-9 optargs)) ; for define*-public

(catch #t
	(lambda () (use-modules (dbi dbi))) ; guile-dbi interface to SQLite3
	(lambda (key . args)
		(format #t "Error: guile-dbi interfaces missing:\n   ~A: ~A: ~A \n"
			key (car args) (cadr args)) #f))

(use-modules (opencog))
(use-modules (opencog matrix))
(use-modules (opencog sheaf))
(use-modules (dbi dbi))

; ---------------------------------------------------------------------
; Convert an integer into a string of upper-case letters. Useful for
; creating link-names.  This prepends the letter "T" to all names, so
; that all MST link-names start with this letter.
; Example:  0 --> TA, 1 --> TB
(define (number->tag num)

	; Convert number to a list of letters.
	(define (number->letters num)
		(define letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
		(unfold-right negative?
			(lambda (i) (string-ref letters (remainder i 26)))
			(lambda (i) (- (quotient i 26) 1))
			num))

	(list->string (cons #\T (number->letters num)))
)

;  ---------------------------------------------------------------------
;
; Return a function that assigns link-names to atom-pairs.
;
; Given two words, the function returns a synthetic link name,
; in the form of a string. The link names are issued in serial
; order, first-come, first-served.
;
(define get-cnr-name
	(let* ((cnt 0)
			(cache (make-afunc-cache
				(lambda (WORD-PAIR)
					(set! cnt (+ cnt 1))
					(number->tag cnt)))))

		; Notice that the lambda does not actually depend on the
		; word-pair. It just issues a new string.  The function
		; cache is what is able to detect and re-emit a previously
		; issued link name.
		;
		; XXX It would be nicer if we could avoid creating the ListLink
		; below... use a pair-caching function of some kind ...
		(lambda (left-word right-word)
			(cache (ListLink left-word right-word)))
	)
)

;  ---------------------------------------------------------------------
;
; Link Grammar expects connectors to be structured in the order of:
;   near- & far- & near+ & far+
; whereas the sections we compute from MST are in the form of
;   far- & near- & near+ & far+
; Thus, for the leftwards-connectors, we have to reverse the order.
;
; Lets recall what a section looks like.
; Here's a real-life example:
;
;    (Section (ctv 1 0 1)
;       (WordNode "an")
;       (ConnectorSeq
;          (Connector
;             (WordNode "interesting")
;             (ConnectorDir "+"))
;          (Connector
;             (WordNode "undertaking")
;             (ConnectorDir "+"))))
;
(define (cset-to-lg-dj GERM CSET)
"
  cset-to-lg-dj GERM CSET
  Return a link-grammar compatible disjunct string for CSET,
  in such a way that it can connect to GERM. Here, GERM should
  be a WordNode or a WordClassNode, and CSET should be a
  ConnectorSeq.
"
	; Get a link-name string identifying this word-pair.
	; The link joins together WORD and GERM.
	; WORD should be a WordNode or a WordClassNode.
	; DIR should be a string, either "+" or "-".
	; Example returned value is the string "TCZKG-".
	(define (cword-to-lg-con WORD DIR)
		(string-append
			(if (equal? DIR "-")
				(get-cnr-name WORD GERM)
				(get-cnr-name GERM WORD)
			)
			DIR
		)
	)

	; Given a list of words, return a link-string that is an
	; or-list of links (links connecting the words to the GERM).
	; Example of a returned value is "(TCZKG- or TVFT- or TCPA-)"
	(define (cword-list-to-lg-con-list WRDLI DIR)
		(if (eq? 1 (length WRDLI))
			(cword-to-lg-con (car WRDLI) DIR)
			(string-append "("
				(fold
					(lambda (WRD STR)
						(string-append STR " or " (cword-to-lg-con WRD DIR)))
					(cword-to-lg-con (car WRDLI) DIR)
					(cdr WRDLI))
				")"))
	)

	; Get a connector, by finding the link that connects WRD-OR-CLA
	; to GERM, and then concatenating the link name with the direction.
	; WRD-OR-CLA should be a WordNode or WordClassNode.
	; DIR should be a string, "+" or "-".
	; hack -- Note that this "broadens" coverage, as flagged up top.
	(define (connector-to-lg-cnr WRD-OR-CLA DIR)
		(define wctype (cog-type WRD-OR-CLA))

		; If its already a word-class...
		(if (eq? 'WordClassNode wctype)
			; ... then just look up the link.
			(cword-to-lg-con WRD-OR-CLA DIR)
			; ... else fold together all classes that the word belongs
			; to into a string. So, if the word belongs to two classes,
			; the resulting string will be an or-list of those two classes.
			(let* ((memb-list (cog-incoming-by-type WRD-OR-CLA 'MemberLink))
					(cls-list (map gdr memb-list)))
				(when (eq? 0 (length cls-list))
					(format #t "Error: Word ~A not in any class\n" WRD-OR-CLA)
					(throw 'bad-membership 'cset-to-lg-dj "Word not in class"))
				(cword-list-to-lg-con-list cls-list DIR)))
	)

	; Link Grammar expects: near- & far- & near+ & far+
	(define (dj-append CONNECTOR dj)
		(define word (gar CONNECTOR))
		(define dir (cog-name (gdr CONNECTOR)))
		(define cnr (connector-to-lg-cnr word dir))
		(if (equal? dir "-")
			(string-append cnr " & " dj)
			(string-append dj " & " cnr)))

	; Create a single string of the connectors, in order.
	; The connectors in SECTION are in the order as noted above:
	;   far- & near- & near+ & far+
	(fold
		(lambda (CNR dj)
			(if dj
				(dj-append CNR dj)
				(connector-to-lg-cnr (gar CNR) (cog-name (gdr CNR)))))
		#f
		(cog-outgoing-set CSET))
)

;  ---------------------------------------------------------------------

; This returns a function that can store a Section to a database.
;
; DB-NAME is the database name to write to.
; LOCALE is the locale to use; e.g EN_us or ZH_cn
; COST-FN is a function that assigns a link-parser cost to each disjunct.
;
; This returns a function that will write sections to the database.
; That is, this creates and returns the function `(lambda (SECTION) ...)`
; so that, when you call it, that section will be saved to the database.
; Calling with #f closes the database.
;
; Example usage:
;    (define add-section (make-db-adder "dict.db" "EN_us" ...))
;    (for-each add-section list-of-sections)
;
(define (make-db-adder DB-NAME LOCALE COST-FN)

	(if (file-exists? DB-NAME)
		(throw 'fail-create 'make-db-adder
			(format #f
				"Error: file '~A' exists; will not over-write.\n\tMaybe you should move it out of the way?" DB-NAME)))

	(define db-obj (dbi-open "sqlite3" DB-NAME))
	(define wrd-id 0)
	(define nprt 0)
	(define is-open #t)
	(define start (current-time))
	(define secs (current-time))
	(define word-cache (make-atom-set))
	(define warn-cnt 0)

	; Escape quotes -- replace single quotes by two successive
	; single-quotes. Example: (escquote "fo'sis'a'blort" 0)
	(define (escquote STR BEG)
		(define pos (string-index STR (lambda (C) (equal? C #\')) BEG))
		(if pos
			(escquote
				(string-replace STR "''" pos pos 1 2)
				(+ pos 2))
			STR))

	; ---------------
	; Insert a single word, with a grammatical class that
	; it belongs to into the dict.
	(define (add-one-word WORD-STR CLASS-STR)

		(define word-str (escquote WORD-STR 0))
		(define class-str (escquote CLASS-STR 0))

		; Oh no!!! Need to fix LEFT-WALL!
		(if (string=? word-str "###LEFT-WALL###")
			(set! word-str "LEFT-WALL"))

		(define query-str
			; Link-grammar SUBSCRIPT_MARK is hex 0x3 aka ASCII #\etx
			(format #f
				"INSERT INTO Morphemes VALUES ('~A', '~A~C~D', '~A');"
				word-str word-str #\etx wrd-id class-str))

		(dbi-query db-obj query-str)

		(when (not (equal? 0 (car (dbi-get_status db-obj))))
			(format #t "sqlite3 failure on query=~A\n" query-str)
			(throw 'fail-insert 'make-db-adder
				(cdr (dbi-get_status db-obj))))
	)

	; ---------------
	; Return a string identifying a word-class.
	(define (mk-cls-str STR)
		; (format #f "<~A>" (escquote STR 0))
		(format #f "{{~A}}" (escquote STR 0)))

	; ---------------
	; Insert either a word, or a word-class, into the dict
	; CLASS-NODE is either a WordNode or a WordClass
	(define (add-word-class CLASS-NODE)
		(define cls-type (cog-type CLASS-NODE))

		; wrd-id serves as a unique ID.
		(set! wrd-id (+ wrd-id 1))

		(cond

			; If we have a word, just invent a word-class for it.
			((eq? cls-type 'WordNode)
				(let ((word-str (cog-name CLASS-NODE)))
					(add-one-word word-str (mk-cls-str word-str))))

			; Loop over all words in the word-class
			((eq? cls-type 'WordClassNode)
				(let ((cls-str (mk-cls-str (cog-name CLASS-NODE))))
					(for-each
						(lambda (memb)
							(add-one-word (cog-name (gar memb)) cls-str))
						(cog-incoming-by-type CLASS-NODE 'MemberLink))))

			; Must be either a WordNode or a WordClassNode
			(else
				(throw 'fail-insert 'make-db-adder
					"Must be either a WordNode or a WordClassNode")))
	)

	; Add connector sets to the database
	(define (add-germ-cset-pair GERM CSET COST)
		(define germ-str (cog-name GERM))
		(define dj-str (cset-to-lg-dj GERM CSET))

		; (format #t "Germ <~A> gets dj=~A\n" germ-str dj-str)
		; Flush periodically
		(set! nprt (+ nprt 1))
		(if (equal? 0 (remainder nprt 5000))
			(begin
				(dbi-query db-obj "END TRANSACTION;")
				(dbi-query db-obj "BEGIN TRANSACTION;")
			))

		; Print progress report
		(if (equal? 0 (remainder nprt 25000))
			(begin
				(format #t "~D done in ~D secs; inserting into <~A>: ~A;\n"
					nprt (- (current-time) secs) germ-str dj-str)
				(set! secs (current-time))
			))

		; Insert the word/word-class (but only if we haven't
		; done so previously.)
		(if (not (word-cache GERM))
			(add-word-class GERM))

		; Insert the disjunct, assigning a cost according
		; to the float-point value returned by the function
		(dbi-query db-obj (format #f
			"INSERT INTO Disjuncts VALUES ('~A', '~A', ~F);"
			(mk-cls-str germ-str) dj-str COST))

		; Might fail with "UNIQUE constraint failed:" so just warn.
		; XXX This is a temp hack, because the classification code
		; is not yet written.
		(let ((err-code (car (dbi-get_status db-obj)))
				(err-msg (cdr (dbi-get_status db-obj))))
			(if (not (equal? 0 err-code))
				(if (string-prefix? "UNIQUE" err-msg)
					(if (< warn-cnt 10)
						(begin
							(set! warn-cnt (+ 1 warn-cnt))
							(format #t "Warning: ~A: Did you forget to classify the connectors?\n"
								err-msg)))
					(throw 'fail-insert 'make-db-adder err-msg))))
	)

	; Add a section to the database.
	(define (add-section SECTION)
		; Ignore CrossSections and other stray markup.
		(if (eq? 'Section (cog-type SECTION))
			(let ((germ (gar SECTION))
					(cset (gdr SECTION))
					(cost (COST-FN SECTION)))
				; Cost will be +inf.0 for sections that have no MI on them.
				; This .. uhh, might be due to a bug in earlier code!?
				(if (< cost 1.0e3)
					(add-germ-cset-pair germ cset cost)))))

	; Write to disk, and close the database.
	(define (shutdown)
		(when is-open
			(set! is-open #f)
			(dbi-query db-obj "END TRANSACTION;")
			(dbi-close db-obj)
			(format #t "Finished inserting ~D records in ~D secs (~6F/sec)\n"
				nprt (- (current-time) start)
				(/ nprt (- (current-time) start)))))

	; Close the DB if an exception is thrown. But otherwise,
	; let the exception pass through to the user.
	(define (raii-add-section SECTION)
		(with-throw-handler #t
			(lambda () (add-section SECTION))
			(lambda (key . args) (shutdown))))

	; Add CLASS as disjunct-collection for unknown words.
	; Typically, CLASS will be a WordClassNode with a lot
	; of sections attached to it.
	(define (add-unknown-word-handler CLASS)

		; wrd-id serves as a unique ID.
		(set! wrd-id (+ wrd-id 1))

		(dbi-query db-obj (format #f
			"INSERT INTO Morphemes VALUES ('<UNKNOWN-WORD>', '<UNKNOWN-WORD.~D>', '~A');"
			wrd-id (mk-cls-str (cog-name CLASS))))

		(if (not (equal? 0 (car (dbi-get_status db-obj))))
			(throw 'fail-insert 'make-db-adder
				(cdr (dbi-get_status db-obj))))
	)

	; Create the tables for words and disjuncts.
	; Refer to the Link Grammar documentation to see a
	; description of this table format. Specifically,
	; take a look at `dict.sql`.
	(dbi-query db-obj (string-append
		"CREATE TABLE Morphemes ( "
		"morpheme TEXT NOT NULL, "
		"subscript TEXT UNIQUE NOT NULL, "
		"classname TEXT NOT NULL);" ))

	(if (not (equal? 0 (car (dbi-get_status db-obj))))
		(throw 'fail-create 'make-db-adder
			(cdr (dbi-get_status db-obj))))

	(dbi-query db-obj
		"CREATE INDEX morph_idx ON Morphemes(morpheme);")

	(dbi-query db-obj (string-append
		"CREATE TABLE Disjuncts ("
		"classname TEXT NOT NULL, "
		"disjunct TEXT NOT NULL, "
		"cost REAL, "
		"UNIQUE(classname,disjunct) );"))

	(dbi-query db-obj
		"CREATE INDEX class_idx ON Disjuncts(classname);")

	(dbi-query db-obj (string-append
		"INSERT INTO Morphemes VALUES ("
		"'<dictionary-version-number>', "
		"'<dictionary-version-number>', "
		"'<dictionary-version-number>');"))

	(dbi-query db-obj (string-append
		"INSERT INTO Disjuncts VALUES ("
		"'<dictionary-version-number>', 'V5v9v0+', 0.0);"))

	(dbi-query db-obj (string-append
		"INSERT INTO Morphemes VALUES ("
		"'<dictionary-locale>', "
		"'<dictionary-locale>', "
		"'<dictionary-locale>');"))

	(dbi-query db-obj (string-append
		"INSERT INTO Disjuncts VALUES ("
		"'<dictionary-locale>', '"
		(string-map (lambda (c) (if (equal? c #\_) #\4 c)) LOCALE)
		"+', 0.0);"))

	; The UNKNOWN-WORD device is needed to make wild-card searches
	; work (when dict debugging). The XXXBOGUS+ will not link to
	; anything. XXX FIXME is this really needed ??
	(dbi-query db-obj (string-append
		"INSERT INTO Morphemes VALUES ("
		"'<UNKNOWN-WORD>', "
		"'<UNKNOWN-WORD>', "
		"'<UNKNOWN-WORD>');"))

	(dbi-query db-obj (string-append
		"INSERT INTO Disjuncts VALUES ("
		"'<UNKNOWN-WORD>', 'XXXBOGUS+', 0.0);"))

	(dbi-query db-obj "PRAGMA synchronous = OFF;")
	(dbi-query db-obj "PRAGMA journal_mode = MEMORY;")
	(dbi-query db-obj "BEGIN TRANSACTION;")

	; Methods on the object
	(lambda (message . args)
		(case message
			((add-section)     (apply raii-add-section args))
			((add-unknown)     (apply add-unknown-word-handler args))
			((shutdown)        (shutdown))
		)
	)
)

;  ---------------------------------------------------------------------

(define*-public (export-csets CSETS DB-NAME LOCALE #:key
	(INCLUDE-UNKNOWN #f))
"
  export-csets CSETS DB-NAME LOCALE

  Write connector sets to a Link Grammar-compatible sqlite3 file.
  CSETS is a matrix containing the connector sets to be written.
  DB-NAME is the database name to write to.
  LOCALE is the locale to use; e.g EN_us or ZH_cn

  Optional keyword: #:INCLUDE-UNKNOWN If set to #t, then each word class
  will also be exported as an UNKNOWN-WORD, allowing the LG parser to use
  this word class when encountering a word that it does not know (i.e.
  is not a part of the vocabulary.)

  Note that link-grammar expects the database file to be called
  \"dict.db\", always!

  Example usage:
     (define pca (make-pseudo-cset-api))
     (define gca (make-gram-class-api pca))
     (export-csets gca \"dict.db\" \"EN_us\")

  In this example, it is assumed that a clustering step has been
  performed, to group words into word-classes. The `gca` object is the
  usual API to wordclass-disjunct pairs.

  Example usage:
     (define pca (make-pseudo-cset-api))
     (define fca (add-subtotal-filter pca 50 50 10 #f))
     (export-csets fca \"dict.db\" \"EN_us\" #:INCLUDE-UNKNOWN #t)

  In this example, it is assumed that NO clustering has been done. Here,
  `pca` is the usual API to word-disjunct pairs.  The subtotal filter
  only admits those sections with a large-enough count. Caution: this
  can result in HUGE dictionaries!
"
	; Create the object that knows where the disuncts are in the
	; atomspace. Create the object that knows how to get the MI
	; of a word-disjunct pair.
	(define psa (add-pair-stars CSETS))
	(define mi-source (add-pair-freq-api psa #:nothrow #t))
	(define looper (add-loop-api psa))

	; Use the MI between word and disjunct as the link-grammar cost
	; LG treats high-cost as "bad", we treat high-MI as "good" so revese
	; the sign.
	(define (cost-fn SECTION)
		(- (mi-source 'pair-fmi SECTION)))

	(define multi-member-classes
		(filter
			(lambda (CLS)
				(< 1 (length (cog-incoming-by-type CLS 'MemberLink))))
			(psa 'left-basis)))

	; Create the SQLite3 database.
	(define dbase (make-db-adder DB-NAME LOCALE cost-fn))
	(define (sectioner SECTION) (dbase 'add-section SECTION))

	(define cnt 0)
	(define (cntr x) (set! cnt (+ cnt 1)))
	(looper 'for-each-pair cntr)
	(format #t "Will store ~D sections\n" cnt)

	; Dump all the connector sets into the database
	(looper 'for-each-pair sectioner)

	(if INCLUDE-UNKNOWN
		(begin
			(format #t "Will store ~D unknown word classes\n"
				(length multi-member-classes))
			(for-each
				(lambda (cls) (dbase 'add-unknown cls))
				multi-member-classes))
		(format #t "Skipping adding unknown-word classes\n"))

	; Close the database
	(dbase 'shutdown)
)

;  ---------------------------------------------------------------------

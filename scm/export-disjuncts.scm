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
;; XX hack alert:
;; TODO support word classes
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
; Given a word-pair atom, return a synthetic link name
; The link names are issued in serial order, first-come, first-served.
;
(define get-cnr-name
	(let ((cnt 0))

		; Notice that the lambda does not actually depend on the
		; word-pair. It just issues a new string.  The function
		; cache is what is able to detect and re-emit a previously
		; issued link name.
		(make-afunc-cache
			(lambda (WORD-PAIR)
				(set! cnt (+ cnt 1))
				(number->tag cnt))))
)

;  ---------------------------------------------------------------------

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
(define (cset-to-lg-dj SECTION)
"
  cset-to-lg-dj - SECTION should be a SectionLink
  Return a link-grammar compatible disjunct string.
"
	(define cnr-to-left (ConnectorDir "-"))

	; The germ of the section (the word)
	(define germ (gar SECTION))

	; Get a link-name identifying this word-pair.
	(define (connector-to-lg-link CONNECTOR)
		(define cnr (gar CONNECTOR))
		(define dir (gdr CONNECTOR))

		(if (equal? dir cnr-to-left)
			(get-cnr-name (ListLink cnr germ))
			(get-cnr-name (ListLink germ cnr))
		)
	)

	; Get a connector, by concatenating the link name with the direction.
	(define (connector-to-lg-cnr CONNECTOR)
		(string-append
			(connector-to-lg-link CONNECTOR)
			(cog-name (gdr CONNECTOR))))

	; Link Grammar expects: near- & far- & near+ & far+
	(define (strappend CONNECTOR dj)
		(define cnr (connector-to-lg-cnr CONNECTOR))
		(if (equal? (gdr CONNECTOR) cnr-to-left)
			(string-append cnr " & " dj)
			(string-append dj " & " cnr)))

	; Create a single string of the connectors, in order.
	; The connectors in SECTION are in the order as noted above:
	;   far- & near- & near+ & far+
	(fold
		(lambda (CNR dj) (if dj (strappend CNR dj)
				(connector-to-lg-cnr CNR)))
		#f
		(cog-outgoing-set (gdr SECTION)))
)

;  ---------------------------------------------------------------------

; This returns a function that can store a Section to a database.
;
; DB-NAME is the databse name to write to.
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

	(let ((db-obj (dbi-open "sqlite3" DB-NAME))
			(wrd-id 0)
			(nprt 0)
			(secs (current-time))
			(word-cache (make-atom-set))
		)

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
		; Insert a single word, with it's grammatical class,
		; into the dict.
		(define (add-one-word WORD-STR CLASS-STR)

			; Oh no!!! Need to fix LEFT-WLL!
			(if (string=? WORD-STR "###LEFT-WALL###")
				(set! WORD-STR "LEFT-WALL"))

			(set! WORD-STR (escquote WORD-STR 0))
			(set! CLASS-STR (escquote CLASS-STR 0))

			(dbi-query db-obj (format #f
				"INSERT INTO Morphemes VALUES ('~A', '~A.~D', '~A');"
				WORD-STR WORD-STR wrd-id CLASS-STR))

			(if (not (equal? 0 (car (dbi-get_status db-obj))))
				(throw 'fail-insert 'make-db-adder
					(cdr (dbi-get_status db-obj))))
		)

		; ---------------
		; Return a string identifying a word-class
		(define (mk-cls-str STR)
			(format #f "<~A.~D>" (escquote STR 0) wrd-id))

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

		; Add data to the database
		(define (add-section SECTION)
			; The germ of the section is either a WordNode or a WordClass
			(define germ (gar SECTION))
			(define germ-str (cog-name germ))
			(define dj-str (cset-to-lg-dj SECTION))

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
			(if (not (word-cache germ))
				(add-word-class germ))

			; Insert the disjunct, assigning a cost according
			; to the float-point value returned by the function
			(dbi-query db-obj (format #f
				"INSERT INTO Disjuncts VALUES ('~A', '~A', ~F);"
				(mk-cls-str germ-str) dj-str (COST-FN SECTION)))

			(if (not (equal? 0 (car (dbi-get_status db-obj))))
				(throw 'fail-insert 'make-db-adder
					(cdr (dbi-get_status db-obj))))
		)

		; Write to disk, and close the database.
		(define (shutdown)
			(format #t "Finished inserting ~D records\n" nprt)
			(dbi-query db-obj "END TRANSACTION;")
			(dbi-close db-obj)
		)

		; Close the DB if an exception is thrown. But otherwise,
		; let the excpetion pass through to the user.
		(define (raii-add-section SECTION)
			(with-throw-handler #t
				(lambda () (add-section SECTION))
				(lambda (key . args) (shutdown))))

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
			"cost REAL );"))

		(dbi-query db-obj
			"CREATE INDEX class_idx ON Disjuncts(classname);")

		(dbi-query db-obj (string-append
			"INSERT INTO Morphemes VALUES ("
			"'<dictionary-version-number>', "
			"'<dictionary-version-number>', "
			"'<dictionary-version-number>');"))

		(dbi-query db-obj (string-append
			"INSERT INTO Disjuncts VALUES ("
			"'<dictionary-version-number>', 'V5v4v0+', 0.0);"))

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
		; anything. `({@T-} & {@T+})` would almost work, except for two
		; reasons: all connectors are upper-case, and the SQL backend
		; does not support optional-braces {} and multi-connectors @.
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

		; Return function that adds data to the database
		; If SECTION if #f, the database is closed.
		(lambda (SECTION)
			(if SECTION
				(raii-add-section SECTION)
				(shutdown))
		))
)

;  ---------------------------------------------------------------------

(define-public (export-csets CSETS DB-NAME LOCALE)
"
  export-csets CSETS DB-NAME LOCALE

  Write connector sets to a Link Grammar-compatible sqlite3 file.
  CSETS is a matrix containing the connector sets to be written.
  DB-NAME is the databse name to write to.
  LOCALE is the locale to use; e.g EN_us or ZH_cn

  Note that link-grammar expects the database file to be called
  \"dict.db\", always!

  Example usage:
     (define gca (make-gram-class-api))
     (export-csets gca \"dict.db\" \"EN_us\")

  In this example, `gca` is the usual API to wordclass-disjunct pairs.
  It's presumed that wordclasses have been previously formed.

  Example usage:
     (define pca (make-pseudo-cset-api))
     (define fca (add-subtotal-filter pca 50 50 10 #f))
     (export-csets fca \"dict.db\" \"EN_us\")

  In this example, `pca` is the usual API to word-disjunct pairs.
  The subtotal filter only admits those sections with a large-enough
  count. Caution: this format can result in HUGE dictionaries!
"
	; Create the object that knows where the disuncts are in the
	; atomspace. Create the object that knows how to get the MI
	; of a word-disjunct pair.
	(define psa (add-pair-stars CSETS))
	(define mi-source (add-pair-freq-api psa))
	(define looper (add-loop-api psa))

	; Use the MI between word and disjunct as the link-grammar cost
	; LG treats high-cost as "bad", we treat high-MI as "good" so revese
	; the sign.
	(define (cost-fn SECTION)
		(- (mi-source 'pair-fmi SECTION)))

	; Create the SQLite3 database.
	(define sectioner (make-db-adder DB-NAME LOCALE cost-fn))

	(define cnt 0)
	(define (cntr x) (set! cnt (+ cnt 1)))
	(looper 'for-each-pair cntr)
	(format #t "Will store ~D csets\n" cnt)

	; Dump all the connector sets into the database
	(looper 'for-each-pair sectioner)

	; Close the database
	(sectioner #f)
)

;  ---------------------------------------------------------------------

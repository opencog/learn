;
; link-class.scm
;
; Merge links into link-classes by grammatical similarity.
; Maximum-entropy style merges.
;
; Copyright (c) 2019 Linas Vepstas
;
;; SPDX-License-Identifier: AGPL-3.0-or-later
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Before link-type merging, Sections contain only pseudo-connectors,
; viz. Connectors with WordNode's in them, referencing another Section
; to connect to. So, for example:
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (WordNode "level")
;             (ConnectorDir "-"))
;          (Connector
;             (WordNode "field")
;             (ConnectorDir "+"))))
;
;
; The goal here is to replace the WordNodes in Connectors by
; LinkClassNodes, which explcitly link to one-another.  That is, the
; WordNodes act as pseudo-connectors: they must link to the head-word
; of a section.  By contrast, a LinkClass is a real link; it must
; link to another LinkClass in another connector (having the same
; name, but the opposite polarity.)



; ---------------------------------------------------------------------

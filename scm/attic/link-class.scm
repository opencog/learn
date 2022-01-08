;
; link-class.scm
;
; Convert pseudo-connector to real connectors.  Merge real links into
; link-classes, by grammatical similarity. Maximum-entropy style merges.
;
; This file describes a link-discovery algorithm that was never
; explored of implemented; the "shapes" idea took it's place.
;
; Copyright (c) 2019 Linas Vepstas
;
;; SPDX-License-Identifier: AGPL-3.0-or-later
; ---------------------------------------------------------------------
; OVERVIEW
; --------
; Before link discovery, Sections contain only pseudo-connectors,
; viz. Connectors with WordNode's in them, referencing another Section
; to connect to. So, for example:
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (WordNode "field")
;             (ConnectorDir "+"))))
;
; The goal here is to replace the WordNodes in Connectors by
; LinkClassNodes, which explcitly link to one-another.  That is, the
; WordNodes act as pseudo-connectors: they must link to the head-word
; of a section.  By contrast, a LinkClass is a real link; it must
; link to another LinkClass in another connector (having the same
; name, but the opposite polarity.)  Thus, for example:
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (LinkClass "ADJ")
;             (ConnectorDir "+"))))
;
;    (Section
;       (WordNode "field")
;       (ConnectorSeq
;          (Connector
;             (LinkClass "ADJ")
;             (ConnectorDir "-"))))
;
; defines a pair of words which can link to each-other with the real
; link (LinkClass "ADJ").
;
; Converting pseudo-connectors to real connectors requires finding
; matched half-links.  For the above example, the matching set of
; half-links would be:
;
;    (Section
;       (WordNode "playing")
;       (ConnectorSeq
;          (Connector
;             (WordNode "field")
;             (ConnectorDir "+"))))
;
;    (Section
;       (WordNode "field")
;       (ConnectorSeq
;          (Connector
;             (WordNode "playing")
;             (ConnectorDir "-"))))
;
; Note that each contains a pseudo-connector that can connect to the
; other Section.  Once such a set of matching half-links is found, it
; is straight-forward to construct a full-link. The hard part is to
; find such pairs in an efficient fashion.
;
; Link Merging
; ------------
; A distinct task is link-merging.  That is, given two links, can the be
; judged to be similar, or not?  This can be accomplished with existing
; tools, provided that a matrix is defined, containing (LinkClass, Seq)
; pairs, with Seq summarizing the connected disjuncts.  The simplest
; way of representing this would seem to be:
;
;    (Section
;       (LinkClass "FOO")
;       (ConnectorSeq
;          (Connector
;             (Lex ...)
;             (ConnectorDir ...))
;          (Connector
;             (AnyNode "Left Link Wild")
;             (ConnectorDir "+"))
;          (Connector
;             (Lex ...)
;             (ConnectorDir ...)))
;       (ConnectorSeq
;          (Connector
;             (Lex ...)
;             (ConnectorDir ...))
;          (Connector
;             (AnyNode "Right Link Wild")
;             (ConnectorDir "-"))
;          (Connector
;             (Lex ...)
;             (ConnectorDir ...)))
;
; Here, (Lex ...) might be either a WordNode, or a LinkClass. Note that
; there are two ConnectorSeq's in the Section: one coming from the left
; disjunct, and one from the right disjunct. The pair of connected
; connectors are denoted by the AnyNodes.
;
; The point of this format is that it allows a standard matrix to be
; defined, so that the collection of (Section (LinkClass "FOO") ...)
; can be compared to (Section (LinkClass "BAR") ...) as vectors, with
; a merge/don't-merge decision made in the usual way (preferably with
; Kulback-Liebler divergence, i.e. symmetric-MI driving that decision.)
;
; A practical problem is that the creation of such LinkClass sections
; threatens to explode the RAM usage on the machine by a factor of five
; or ten or so, and thus even the "micro" datasets may explode to a
; large size.  Thus, its best to create these sections incrementally,
; working only with the highest-count sections at first.
;
; By default, the count on such LinkClass sections is defined as the
; product of the counts of the individual WordNode sections; during
; merger, these counts would get added.


; ---------------------------------------------------------------------

ADD_GUILE_MODULE (FILES
	learn.scm
	agglo-rank.scm
	batch-pair.scm
	common.scm
	gram-class-api.scm
	gram-classification.scm
	gram-majority.scm
	gram-optim.scm
	gram-pairwise.scm
	gram-projective.scm
	in-group.scm
	lg-compare.scm
	link-class.scm
	mst-parser.scm
	pair-count-window.scm
	pseudo-csets.scm
	shape-project.scm
	shape-vec.scm
	utilities.scm
	word-pair-count.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/learn"
)

# A distinct module for export-to-link-grammar
ADD_GUILE_MODULE (FILES
	lg-export.scm
	export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

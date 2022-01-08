ADD_GUILE_MODULE (FILES
	learn.scm
	pair-count/batch-pair.scm
	pair-count/pair-count-window.scm
	pair-count/word-pair-count.scm
	agglo-rank.scm
	common.scm
	gram-class-api.scm
	gram-classification.scm
	gram-majority.scm
	gram-optim.scm
	in-group.scm
	lg-compare.scm
	link-class.scm
	mst-parser.scm
	pseudo-csets.scm
	shape-project.scm
	shape-vec.scm
	utilities.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/learn"
)

# A distinct module for export-to-link-grammar
ADD_GUILE_MODULE (FILES
	lg-export.scm
	export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

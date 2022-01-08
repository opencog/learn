ADD_GUILE_MODULE (FILES
	learn.scm
	common.scm

	pair-count/batch-pair.scm
	pair-count/pair-count-window.scm
	pair-count/word-pair-count.scm

	mst-parse/mst-parser.scm
	mst-parse/pseudo-csets.scm

	gram-class/agglo-rank.scm
	gram-class/gram-class-api.scm
	gram-class/gram-classification.scm
	gram-class/gram-majority.scm
	gram-class/gram-optim.scm
	gram-class/in-group.scm
	gram-class/shape-project.scm
	gram-class/shape-vec.scm

	lg-compare.scm
	utilities.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/learn"
)

# A distinct module for export-to-link-grammar
ADD_GUILE_MODULE (FILES
	lg-export.scm
	export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

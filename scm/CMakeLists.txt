ADD_GUILE_MODULE (FILES
	learn.scm
	common.scm
	utilities.scm

	pair-count/batch-pair.scm
	pair-count/pair-count-window.scm
	pair-count/word-pair-count.scm

	mst-parse/mst-parser.scm
	mst-parse/pseudo-csets.scm

	gram-class/agglo-rank.scm
	gram-class/gram-class-api.scm
	gram-class/gram-filters.scm
	gram-class/gram-majority.scm
	gram-class/gram-optim.scm
	gram-class/in-group.scm
	gram-class/jaccard.scm
	gram-class/log-merge.scm
	gram-class/mi-similarity.scm
	gram-class/shape-project.scm
	gram-class/shape-vec.scm
	gram-class/similarity.scm
	gram-class/singletons.scm

	lg-compare.scm

	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/learn"
)

# A distinct module for export-to-link-grammar
# Why? Because it has some ugly LG and sqlite3 dependencies.
ADD_GUILE_MODULE (FILES
	lg-export/lg-export.scm
	lg-export/export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

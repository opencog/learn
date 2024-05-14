ADD_GUILE_MODULE (FILES
	learn.scm
	common.scm
	utils/utilities.scm
	utils/waiter.scm

	pipe-parse/pipe-count.scm

	pair-count/pair-api.scm
	pair-count/word-pair-pipe.scm

	parse/lg-pipe-parser.scm
	parse/pseudo-csets.scm
	parse/bond-links.scm
	parse/sliding-block.scm

	gram-class/agglo-goe.scm
	gram-class/agglo-mi-rank.scm
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

# ------ The End  --------

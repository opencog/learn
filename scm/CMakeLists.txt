ADD_GUILE_MODULE (FILES
	learn.scm
	agglo-rank.scm
	batch-word-pair.scm
	cliques.scm
	common.scm
	gram-class-api.scm
	gram-classification.scm
	gram-optim.scm
	gram-pairwise.scm
	gram-projective.scm
	lg-compare.scm
	link-class.scm
	link-pipeline.scm
	mst-parser.scm
	pseudo-csets.scm
	shape-project.scm
	shape-vec.scm
	utilities.scm
	vectors.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/learn"
)

# A distinct module for export-to-link-grammar
ADD_GUILE_MODULE (FILES
	lg-export.scm
	export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

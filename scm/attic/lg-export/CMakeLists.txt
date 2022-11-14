
# A distinct module for export-to-link-grammar
# Why? Because it has some ugly LG and sqlite3 dependencies.
ADD_GUILE_MODULE (FILES
	lg-export/lg-export.scm
	lg-export/export-disjuncts.scm
	MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/nlp/lg-export"
)

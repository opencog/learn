
INSTALL(FILES
	cogserver-gram.scm
	cogserver-lg.scm
	cogserver-mst.scm
	cogserver.scm
	file-block-process.sh
	file-line-process.sh
	file-split-process.sh
	file-xform-process.sh
	halt-all.sh
	marginals-mst.scm
	marginals-mst-shape.scm
	marginals-pair.scm
	process-corpus.sh
	README.md
	renice.sh
	socket-send.pl
	split-sentences.pl
	submit-block.pl
	submit-lines.pl
	trim-pair.scm
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run-common
)


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

INSTALL(FILES
	../run-config/0-pipeline.sh
	../run-config/2-pair-conf.sh
	../run-config/3-mpg-conf.sh
	../run-config/4-gram-conf.sh
	../run-config/README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run-config
)

INSTALL(FILES
	../run-config/dict-combined/storage.dict
	../run-config/dict-combined/README.md
	../run-config/dict-combined/4.0.affix
	DESTINATION ${COGUTIL_DATA_DIR}/../link-grammar/dict-combined
)

INSTALL(FILES
	../run-config/dict-pair/storage.dict
	../run-config/dict-pair/README.md
	../run-config/dict-pair/4.0.affix
	DESTINATION ${COGUTIL_DATA_DIR}/../link-grammar/dict-pair
)


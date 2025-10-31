
INSTALL(FILES
	README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run-common
)

INSTALL(PROGRAMS
	cogserver-gram.scm
	cogserver-lg.scm
	cogserver-mst.scm
	cogserver-pair.scm
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

INSTALL(FILES
	../run/README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run
)

INSTALL(PROGRAMS
	../run/run-tmux.sh
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run
)

INSTALL(FILES
	../run/2-word-pairs/README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/2-word-pairs
)

INSTALL(PROGRAMS
	../run/2-word-pairs/pair-submit.sh
	../run/2-word-pairs/trim-dataset.sh
	../run/2-word-pairs/compute-marginals.sh
	../run/2-word-pairs/run-shells.sh
	../run/2-word-pairs/halt.sh
	../run/2-word-pairs/run-all.sh
	../run/2-word-pairs/run-cogserver.sh
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/2-word-pairs
)

INSTALL(FILES
	../run/3-mst-parsing/README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/3-mst-parsing
)

INSTALL(PROGRAMS
	../run/3-mst-parsing/run-mst-shells.sh
	../run/3-mst-parsing/run-mst-cogserver.sh
	../run/3-mst-parsing/mst-submit.sh
	../run/3-mst-parsing/run-all-mst.sh
	../run/3-mst-parsing/compute-mst-marginals.sh
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/3-mst-parsing
)

INSTALL(FILES
	../run/4-gram-class/README.md
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/4-gram-class
)

INSTALL(PROGRAMS
	../run/4-gram-class/run-all-gram.sh
	../run/4-gram-class/run-gram-cogserver.sh
	DESTINATION ${COGUTIL_DATA_DIR}/learn/run/4-gram-class
)

# ----------- the end -----------

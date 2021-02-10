CMAKE_MINIMUM_REQUIRED(VERSION 2.8.12.2)

PROJECT(learn)

# Cogutil
FIND_PACKAGE(CogUtil 2.0.3 CONFIG REQUIRED)
IF(COGUTIL_FOUND)
	ADD_DEFINITIONS(-DHAVE_COGUTIL)
	SET(HAVE_COGUTIL 1)

	# Add the 'cmake' directory from cogutil to search path
	list(APPEND CMAKE_MODULE_PATH  ${COGUTIL_DATA_DIR}/cmake)
ENDIF()

# AtomSpace
FIND_PACKAGE(AtomSpace 5.0.4 CONFIG REQUIRED)
IF(ATOMSPACE_FOUND)
	ADD_DEFINITIONS(-DHAVE_ATOMSPACE)
	SET(HAVE_ATOMSPACE 1)
ENDIF()

# CogServer
FIND_PACKAGE(CogServer 1.0.0 CONFIG REQUIRED)
IF (COGSERVER_FOUND)
	ADD_DEFINITIONS(-DHAVE_COGSERVER)
	SET(HAVE_COGSERVER 1)
ENDIF (COGSERVER_FOUND)

# Guile
FIND_PACKAGE(Guile 2.2.2 REQUIRED)
IF(GUILE_FOUND)
	ADD_DEFINITIONS(-DHAVE_GUILE)
	SET(HAVE_GUILE 1)
ENDIF()

# Load cmake functions defined in cogutil repo that depend on Guile.
IF(GUILE_FOUND AND COGUTIL_FOUND)
	INCLUDE("${COGUTIL_DATA_DIR}/cmake/OpenCogMacros.cmake")
	INCLUDE("${COGUTIL_DATA_DIR}/cmake/OpenCogGuile.cmake")
ENDIF()

ADD_CUSTOM_TARGET(db
	# Reset all test databases needed
	VERBATIM
	COMMAND bash -c "dropdb --if-exists ULL_tests"
	COMMAND bash -c "createdb ULL_tests"
	COMMAND bash -c "cat ${CMAKE_SOURCE_DIR}/run-poc/atom.sql | psql ULL_tests"
	COMMAND bash -c "echo 'Created database ULL_tests.'"

	COMMAND bash -c "dropdb --if-exists ULL_calcMI_clique_test"
	COMMAND bash -c "createdb ULL_calcMI_clique_test"
	COMMAND bash -c "cat ${CMAKE_SOURCE_DIR}/run-poc/atom.sql | psql ULL_calcMI_clique_test"
	COMMAND bash -c "echo 'Created database ULL_calcMI_clique_test.'"

	COMMAND bash -c "dropdb --if-exists ULL_calcMI_any_test"
	COMMAND bash -c "createdb ULL_calcMI_any_test"
	COMMAND bash -c "cat ${CMAKE_SOURCE_DIR}/run-poc/atom.sql | psql ULL_calcMI_any_test"
	COMMAND bash -c "echo 'Created database ULL_calcMI_any_test.'"

	# The current ULL pipeline is tokenization-agnostic, so the input
	# should be pre-tokenized by some other method. The pipeline only
	# splits sentences by spaces. For the tests to work, we need to
	# remove the content of the affix-file in the link-grammar dictionary
	# for "any" language. For the test suite, it is done below.
	COMMAND bash -c "echo ''; echo 'Backing up and clearing affix file for LG any'"
	COMMAND bash -c "if [ ! -f /usr/local/share/link-grammar/any/affix-punc-original ]; then sudo cp /usr/local/share/link-grammar/any/affix-punc /usr/local/share/link-grammar/any/affix-punc-original; fi"
	COMMAND bash -c "echo '' > tmp; sudo mv tmp /usr/local/share/link-grammar/any/affix-punc"
)

ADD_SUBDIRECTORY (run-poc)
ADD_SUBDIRECTORY (scm)
ADD_SUBDIRECTORY (tests)
ENABLE_TESTING()
MESSAGE("Before running 'make test', make sure to 'make db'")

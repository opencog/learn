# These tests rely on psql storage.
# If you are having problems related to the database, please make
# sure you have properly set up psql, as indicated in the atomspace
# repository, in atomspace/opencog/persist/sql/README.md
# Set environment variables PGUSER and PGPASSWORD below 
# to the values used during psql setup.

# If you're using an opencog docker container, psql should
# already be set up.
# Set postgress credentials
if(NOT DEFINED ENV{PGUSER})
	SET(ENV{PGUSER} $ENV{USER})
endif(NOT DEFINED ENV{PGUSER})
SET(ENV{PGPASSWORD} "cheese")


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

ENABLE_TESTING()
MESSAGE("Before running 'make test', make sure to 'make db'")

# Add Unit tests
ADD_GUILE_TEST(UllPairCounting pair-count-tests.scm)
ADD_GUILE_TEST(UllMSTParsing MST-parse-test.scm)
ADD_GUILE_TEST(UllMICalcClique MI-calc-test-clique.scm)
ADD_GUILE_TEST(UllMICalcAny MI-calc-test-any.scm)

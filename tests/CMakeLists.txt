# These tests rely on psql storage.
# If you are having problems related to the database, please make
# sure you have properly set up psql, as indicated in the atomspace
# repository, in atomspace/opencog/persist/sql/README.md
# Set environment variables PGUSER and PGPASSWORD below 
# to the values used during psql setup.

# If you're using an opencog docker container, psql should
# already be set up.

ENABLE_TESTING()

# Set postgress credentials
if(NOT DEFINED ENV{PGUSER})
	SET(ENV{PGUSER} $ENV{USER})
endif(NOT DEFINED ENV{PGUSER})
SET(ENV{PGPASSWORD} "cheese")

# Reset all test databases needed
message("\nIf database doesn't exist, dropdb sends error message. Disregard it.\n")
execute_process(COMMAND "dropdb" "ULL_tests")
execute_process(COMMAND "createdb" "ULL_tests")
execute_process(COMMAND "psql" "ULL_tests" INPUT_FILE "${CMAKE_SOURCE_DIR}/run-poc/atom.sql")
message("\nCreated database ULL_tests.\n")

execute_process(COMMAND "dropdb" "ULL_calcMI_clique_test")
execute_process(COMMAND "createdb" "ULL_calcMI_clique_test")
execute_process(COMMAND "psql" "ULL_calcMI_clique_test" INPUT_FILE "${CMAKE_SOURCE_DIR}/run-poc/atom.sql")
message("\nCreated database ULL_calcMI_clique_test.\n")

execute_process(COMMAND "dropdb" "ULL_calcMI_any_test")
execute_process(COMMAND "createdb" "ULL_calcMI_any_test")
execute_process(COMMAND "psql" "ULL_calcMI_any_test" INPUT_FILE "${CMAKE_SOURCE_DIR}/run-poc/atom.sql")
message("\nCreated database ULL_calcMI_any_test.\n")

# The current ULL pipeline is tokenization-agnostic, so the input
# should be pre-tokenized by some other method. The pipeline only
# splits sentences by spaces. For the tests to work, we need to
# remove the content of the affix-file in the link-grammar dictionary
# for "any" language. For the test suite, it is done below.
execute_process(COMMAND "echo ''" OUTPUT_FILE "usr/local/share/link-grammar/any/affix-punc")

# Add Unit tests
ADD_GUILE_TEST(UllPairCounting pair-count-tests.scm)
ADD_GUILE_TEST(UllMSTParsing MST-parse-test.scm)
ADD_GUILE_TEST(UllMICalcClique MI-calc-test-clique.scm)
ADD_GUILE_TEST(UllMICalcAny MI-calc-test-any.scm)

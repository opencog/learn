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

ENABLE_TESTING()

# Add Unit tests
ADD_GUILE_TEST(UllPairCounting pair-count-tests.scm)
ADD_GUILE_TEST(UllMSTParsing MST-parse-test.scm)
ADD_GUILE_TEST(UllMICalcClique MI-calc-test-clique.scm)
ADD_GUILE_TEST(UllMICalcAny MI-calc-test-any.scm)

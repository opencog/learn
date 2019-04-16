ENABLE_TESTING()

ADD_GUILE_TEST(UllPairCounting pair-count-tests.scm)
ADD_GUILE_TEST(UllMICalcClique MI-calc-test-clique.scm)
ADD_GUILE_TEST(UllMICalcAny MI-calc-test-any.scm)
ADD_GUILE_TEST(UllMSTParsing MST-parse-test.scm)

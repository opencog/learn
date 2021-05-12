
# Unit tests
ENABLE_TESTING()

# re-enable this someday...!?
# ADD_SUBDIRECTORY (old)

# Add Unit tests
ADD_GUILE_TEST(ConnectorMergeBasic  connector-merge-basic.scm)
ADD_GUILE_TEST(ConnectorMergeCons   connector-merge-cons.scm)
ADD_GUILE_TEST(ConnectorMergeTri    connector-merge-tri.scm)
ADD_GUILE_TEST(ConnectorMergeTriCon connector-merge-tricon.scm)


# Unit tests
ENABLE_TESTING()

# re-enable this someday...!?
# ADD_SUBDIRECTORY (old)

# Add Unit tests
ADD_GUILE_TEST(ConnectorMergeBasic connector-merge-basic.scm)

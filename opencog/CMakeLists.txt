
#DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog as-io-config" "ATOMSPACE_IO_TEST")

ADD_SUBDIRECTORY (io)

# Testing framework boilerplate.
#WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-io-config.scm SCM_CONFIG TRUE)
#WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/as-io-config-installable.scm SCM_CONFIG FALSE)
#INSTALL(FILES ${GUILE_BIN_DIR}/opencog/as-io-config-installable.scm
#DESTINATION ${GUILE_SITE_DIR}/opencog RENAME as-io-config.scm)


DECLARE_GUILE_CONFIG_TARGET(SCM_CONFIG "opencog sensory-config" "SENSORY_TEST")

ADD_SUBDIRECTORY (sensory-types)
ADD_SUBDIRECTORY (sensory)

# Testing framework boilerplate.
WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/sensory-config.scm SCM_CONFIG TRUE)
WRITE_GUILE_CONFIG(${GUILE_BIN_DIR}/opencog/sensory-config-installable.scm SCM_CONFIG FALSE)
INSTALL(FILES ${GUILE_BIN_DIR}/opencog/sensory-config-installable.scm
DESTINATION ${GUILE_SITE_DIR}/opencog RENAME sensory-config.scm)

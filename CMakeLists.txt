CMAKE_MINIMUM_REQUIRED(VERSION 3.12)
SET(CMAKE_INSTALL_MESSAGE LAZY)

PROJECT(learn)

# Cogutil
FIND_PACKAGE(CogUtil 2.0.3 CONFIG REQUIRED)
IF(COGUTIL_FOUND)
	SET(HAVE_COGUTIL 1)

	# Add the 'cmake' directory from cogutil to search path
	list(APPEND CMAKE_MODULE_PATH  ${COGUTIL_DATA_DIR}/cmake)

	# Needed to set OC_CMAKE_DIR
	if (NOT DEFINED ATOMSPACE_DATA_DIR)
		set (ATOMSPACE_DATA_DIR "${COGUTIL_DATA_DIR}")
	endif (NOT DEFINED ATOMSPACE_DATA_DIR)

	include(OpenCogGccOptions)
	include(OpenCogLibOptions)
	include(OpenCogInstallOptions)
	include(Summary)

ENDIF()

# AtomSpace
FIND_PACKAGE(AtomSpace 5.0.4 CONFIG REQUIRED)
IF(ATOMSPACE_FOUND)
	SET(HAVE_ATOMSPACE 1)
ENDIF()

# CogServer
FIND_PACKAGE(CogServer 1.0.0 CONFIG REQUIRED)
IF (COGSERVER_FOUND)
	SET(HAVE_COGSERVER 1)
ENDIF (COGSERVER_FOUND)

# RocksDB
FIND_PACKAGE(AtomSpaceRocks 1.3.0 CONFIG REQUIRED)
IF (ATOMSPACE_ROCKS_FOUND)
	SET(HAVE_ATOMSPACE_ROCKS 1)
ENDIF (ATOMSPACE_ROCKS_FOUND)

# ----------------------------------------------------------
# Guile Python and Cython

include(OpenCogFindGuile)
# include(OpenCogFindPython)

# Load cmake functions defined in cogutil repo that depend on Guile.
INCLUDE(OpenCogMacros)
INCLUDE(OpenCogGuile)
INCLUDE(OpenCogCython)

# ----------------------------------------------------------
ADD_SUBDIRECTORY (fake)
ADD_SUBDIRECTORY (scm)
ADD_SUBDIRECTORY (run-common)

# Unit tests
ADD_CUSTOM_TARGET(tests)
ADD_SUBDIRECTORY (tests EXCLUDE_FROM_ALL)
ADD_CUSTOM_TARGET(test
	DEPENDS tests
	WORKING_DIRECTORY tests
	COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
	COMMENT "Running tests..."
)
ADD_CUSTOM_TARGET(check
	DEPENDS tests
	WORKING_DIRECTORY tests
	COMMAND ${CMAKE_CTEST_COMMAND} $(ARGS)
	COMMENT "Running tests..."
)

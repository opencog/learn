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

# Guile
FIND_PACKAGE(Guile 2.2.2 REQUIRED)
IF(GUILE_FOUND)
  ADD_DEFINITIONS(-DHAVE_GUILE)
  SET(HAVE_GUILE 1)
ENDIF()

# Load cmake functions defined in cogutil repo that depend on Guile.
IF(GUILE_FOUND AND COGUTIL_FOUND)
  INCLUDE("${COGUTIL_DATA_DIR}/cmake/OpenCogFunctions.cmake")
ENDIF()

# Used for configuring experimental setup. Run 'make run-ull' from build
# directory to configure the setup for expirmenting with code in run-poc.
# NOTE: The order of the command matters, so test after making changes.
ADD_CUSTOM_TARGET(run-ull
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/run-poc"
    "${PROJECT_BINARY_DIR}/run-ull"
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/run/nonbreaking_prefixes/"
    "${PROJECT_BINARY_DIR}/run-ull/nonbreaking_prefixes/"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/run/split-sentences.pl"
    "${PROJECT_BINARY_DIR}/run-ull/"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/run/config/*"
    "${PROJECT_BINARY_DIR}/run-ull/config"
  WORKING_DIRECTORY ${CURRENT_SOURCE_DIR}
  COMMENT "Setting run-ull in ${PROJECT_BINARY_DIR}/run-ull"
)

ADD_SUBDIRECTORY (scm)
ADD_SUBDIRECTORY (tests)
ENABLE_TESTING()

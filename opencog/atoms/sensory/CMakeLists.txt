
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (sensory SHARED
	FileReadNode.cc
)

# Without this, parallel make will race and crap up the generated files.
ADD_DEPENDENCIES(sensory sensory_atom_types)

TARGET_LINK_LIBRARIES(sensory
	sensory-types
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS sensory EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FileReadNode.h
	DESTINATION "include/opencog/atoms/sensory"
)

add_guile_extension(SCM_CONFIG sensory "opencog-ext-path-sensory")

add_guile_module(FILES
   sensory.scm
   ${CMAKE_CURRENT_BINARY_DIR}/../sensory-types/sensory_types.scm
   MODULE_DESTINATION "${GUILE_SITE_DIR}/opencog/sensory"
   DEPENDS sensory_atom_types
)

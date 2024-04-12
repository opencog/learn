
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (sensory
	FileIO.cc
)

# Without this, parallel make will race and crap up the generated files.
#ADD_DEPENDENCIES(sensory opencog_atom_types)

TARGET_LINK_LIBRARIES(sensory
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS sensory EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FileIO.h
	DESTINATION "include/opencog/atoms/sensory"
)

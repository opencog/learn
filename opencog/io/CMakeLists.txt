
# The atom_types.h file is written to the build directory
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR})

ADD_LIBRARY (atomio
	FileIO.cc
)

# Without this, parallel make will race and crap up the generated files.
#ADD_DEPENDENCIES(atomio opencog_atom_types)

TARGET_LINK_LIBRARIES(atomio
	${ATOMSPACE_LIBRARIES}
	${COGUTIL_LIBRARY}
)

INSTALL (TARGETS atomio EXPORT AtomSpaceTargets
	DESTINATION "lib${LIB_DIR_SUFFIX}/opencog"
)

INSTALL (FILES
	FileIO.h
	DESTINATION "include/opencog/atoms/io"
)

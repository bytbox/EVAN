# TODO deal with out-of-source builds

set (DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set (DOXYGEN_INPUTS "${CMAKE_SOURCE_DIR}/core ${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/interp ${CMAKE_SOURCE_DIR}/util")

find_package(Doxygen)

if (DOXYGEN_FOUND)
	configure_file (Doxyfile.in Doxyfile)
	add_custom_target (
		html ALL
		${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile )
endif ()


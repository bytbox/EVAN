set (DOXYGEN_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set (DOXYGEN_INPUTS "${CMAKE_SOURCE_DIR}/core ${CMAKE_SOURCE_DIR}/include ${CMAKE_SOURCE_DIR}/interp ${CMAKE_SOURCE_DIR}/util ${CMAKE_SOURCE_DIR}/parse ${CMAKE_SOURCE_DIR}/typecheck ${CMAKE_SOURCE_DIR}/output")

find_package(Doxygen)

if (DOXYGEN_FOUND)
	configure_file (Doxyfile.in Doxyfile)
	add_custom_target (
		doc-html ALL
		${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile )
endif ()


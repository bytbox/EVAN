find_package (Qt4)

if (QT_FOUND)
	include (${QT_USE_FILE})
	add_definitions (${QT_DEFINITIONS})

	file (GLOB GUISRCS *.cc)
	file (GLOB GUIHDRS *.hh)

	QT4_WRAP_CPP (GUIHDRS_MOC ${GUIHDRS})

	include_directories (${QT_INCLUDE_DIR})
	add_executable (evan-gui ${GUISRCS} ${GUIHDRS_MOC})
	target_link_libraries (evan-gui core typecheck interp util ${QT_LIBRARIES})
endif ()


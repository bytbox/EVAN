add_custom_command (
	OUTPUT builtins.cc
	COMMAND ${CMAKE_SOURCE_DIR}/build/gen-builtins-cc.pl ${CMAKE_CURRENT_SOURCE_DIR}/builtins.xml builtins.cc
	DEPENDS builtins.xml
	COMMENT "Generating builtins.cc"
)

file (GLOB CORESRCS "*.cc")
add_library (core ${CORESRCS} builtins.cc)


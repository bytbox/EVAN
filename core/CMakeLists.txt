add_custom_command (
	OUTPUT foreigns.cc
	COMMAND ${CMAKE_SOURCE_DIR}/build/gen-foreigns-cc.pl ${CMAKE_CURRENT_SOURCE_DIR}/foreigns.xml foreigns.cc
	DEPENDS foreigns.xml ${CMAKE_SOURCE_DIR}/build/gen-foreigns-cc.pl
	COMMENT "Generating foreigns.cc"
)

add_custom_command (
	OUTPUT builtins.cc
	COMMAND ${CMAKE_SOURCE_DIR}/build/gen-builtins-cc.pl ${CMAKE_CURRENT_SOURCE_DIR}/builtins.xml builtins.cc
	DEPENDS builtins.xml ${CMAKE_SOURCE_DIR}/build/gen-builtins-cc.pl
	COMMENT "Generating builtins.cc"
)

file (GLOB CORESRCS "*.cc")
add_library (core ${CORESRCS} builtins.cc foreigns.cc)


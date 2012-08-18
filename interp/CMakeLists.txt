add_custom_command (
	OUTPUT foreigns.cc
	COMMAND ${CMAKE_SOURCE_DIR}/tools/gen-interp-foreigns-cc.pl ${CMAKE_CURRENT_SOURCE_DIR}/../foreign/foreigns.xml foreigns.cc
	DEPENDS ../foreign/foreigns.xml ${CMAKE_SOURCE_DIR}/tools/gen-interp-foreigns-cc.pl
	COMMENT "Generating foreigns.cc"
)

file (GLOB INTERPSRCS "*.cc" "builtin/*.cc")
add_library (interp ${INTERPSRCS} foreigns.cc)


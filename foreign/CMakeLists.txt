file (GLOB_RECURSE FOREIGNSRCS "*.c")
include_directories (${CMAKE_CURRENT_SOURCE_DIR})
add_library (foreign ${FOREIGNSRCS})


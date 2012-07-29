# TODO support clang
# TODO generate ctags
# TODO benchmarks and profiling

cmake_minimum_required (VERSION 2.6)

project (EVAN)
set (EVAN_VERSION 0.0.1)

set (CMAKE_MODULE_PATH "${EVAN_SOURCE_DIR}/cmake/")

find_package (CCACHE)
if (NOT CCACHE_FOUND)
	message (WARNING "ccache not found - compilation will be slow")
endif()

message (${CMAKE_CXX_COMPILER})
set (CMAKE_CXX_COMPILER_ARG1 ${CMAKE_CXX_COMPILER})
set (CMAKE_CXX_COMPILER ${CCACHE_EXECUTABLE})

set (CMAKE_BUILD_TYPE Debug)

set (CMAKE_CXX_FLAGS "-Wall -Wswitch-enum -Wunused -Wuninitialized -std=c++11")
set (CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set (CMAKE_CXX_FLAGS_RELEASE "-O3")

include_directories (${EVAN_SOURCE_DIR}/include ${EVAN_BINARY_DIR}/include)

add_subdirectory (include)
add_subdirectory (doc)

add_subdirectory (core)
add_subdirectory (interp)
add_subdirectory (parse)
add_subdirectory (typecheck)
add_subdirectory (compile)
add_subdirectory (util)

add_subdirectory (cli)
add_subdirectory (gui)
add_subdirectory (httpd)

enable_testing ()
add_subdirectory (test)

add_custom_target (full DEPENDS core interp util evan-gui doc-html check)


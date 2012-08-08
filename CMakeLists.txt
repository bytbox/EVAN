# TODO support clang
# TODO generate ctags
# TODO benchmarks

cmake_minimum_required (VERSION 2.6)

project (EVAN)
set (EVAN_VERSION 0.0.1)

#set (CMAKE_MODULE_PATH "${EVAN_SOURCE_DIR}/cmake/")

if (${PROFILE})
	set(C_FLAGS_PROFILE "")
	set(CXX_FLAGS_PROFILE "")
	set(LINKER_FLAGS_PROFILE "")
endif()
if (${COVERAGE})
	set(C_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
	set(CXX_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
	set(LINKER_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
endif()

set (CMAKE_BUILD_TYPE Debug)

set (CMAKE_C_FLAGS "-Wall -Wswitch-enum -Wunused -Wuninitialized ${C_FLAGS_PROFILE} ${C_FLAGS_COVERAGE}")
set (CMAKE_C_FLAGS_DEBUG "-g -O0")
set (CMAKE_C_FLAGS_RELEASE "-O3")

set (CMAKE_CXX_FLAGS "-Wall -Wswitch-enum -Wunused -Wuninitialized -std=c++11 ${CXX_FLAGS_PROFILE} ${CXX_FLAGS_COVERAGE}")
set (CMAKE_CXX_FLAGS_DEBUG "-g -O0")
set (CMAKE_CXX_FLAGS_RELEASE "-O3")
set (CMAKE_EXE_LINKER_FLAGS "${LINKER_FLAGS_PROFILE} ${LINKER_FLAGS_COVERAGE}")

include_directories (${EVAN_SOURCE_DIR}/include ${EVAN_BINARY_DIR}/include)

add_subdirectory (include)
add_subdirectory (doc)

add_subdirectory (core)
add_subdirectory (interp)
add_subdirectory (parse)
add_subdirectory (typecheck)
add_subdirectory (compile)
add_subdirectory (foreign)
add_subdirectory (util)

add_subdirectory (cli)
add_subdirectory (gui)
add_subdirectory (httpd)

enable_testing ()
add_subdirectory (test)

add_custom_target (full DEPENDS core interp util evan-gui evani evanc evan-doc evan-httpd doc-html check)


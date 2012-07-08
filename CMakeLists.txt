# TODO generate ctags
# TODO doxygen
# TODO pass version as definition
# TODO use separate build dir

cmake_minimum_required (VERSION 2.6)

project (EVAN)
set (EVAN_VERSION 0.0.1)

set (CMAKE_BUILD_TYPE Debug)

set (CMAKE_CXX_FLAGS "-Wall -Wswitch-enum -Wunused -Wuninitialized -std=c++11")
set (CMAKE_CXX_FLAGS_DEBUG "-std=c++11 -g -O0")
set (CMAKE_CXX_FLAGS_RELEASE "-O3")

include_directories (${EVAN_SOURCE_DIR}/include)

add_subdirectory (include)
add_subdirectory (doc)

add_subdirectory (core)
add_subdirectory (interp)
add_subdirectory (util)

add_subdirectory (gui)
add_subdirectory (httpd)
add_subdirectory (test)

add_executable (evan evan.cc)
target_link_libraries (evan core interp util)


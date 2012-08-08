file (GLOB_RECURSE HTTPDSRCS *.cc)

add_executable (evan-httpd ${HTTPDSRCS})
target_link_libraries (evan-httpd util)


# TODO memory checking
# TODO code coverage

include_directories (${CMAKE_CURRENT_SOURCE_DIR}/common)

file (GLOB_RECURSE TESTSRCS "*.cc")
add_executable (evan-test EXCLUDE_FROM_ALL ${TESTSRCS})
target_link_libraries (evan-test core interp util compile parse typecheck)
add_test (evan-test evan-test)

add_custom_target (check COMMAND evan-test DEPENDS evan-test COMMENT "Running evan-test" VERBATIM)


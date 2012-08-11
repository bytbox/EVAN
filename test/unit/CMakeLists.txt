# TODO memory checking
# TODO code coverage

include_directories (${CMAKE_CURRENT_SOURCE_DIR}/common)

file (GLOB_RECURSE TESTSRCS "*.cc")
add_executable (evan-test EXCLUDE_FROM_ALL ${TESTSRCS})
target_link_libraries (evan-test parse core interp util compile typecheck foreign)

add_custom_target (
	check-unit
	COMMAND evan-test ${CMAKE_CURRENT_SOURCE_DIR}
	DEPENDS evan-test
	COMMENT "Running evan-test"
	VERBATIM
	)


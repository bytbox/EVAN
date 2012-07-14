# TODO multiplex tests
# TODO memory checking
# TODO code coverage

add_subdirectory(common)
include_directories (${CMAKE_CURRENT_SOURCE_DIR}/common)

set (TESTLIBS core interp typecheck compile util)

function (add_tests NAME PATTERN)
	file (GLOB TEST_FILES ${PATTERN})
	foreach (TESTFILE ${TEST_FILES})
		get_filename_component (TEST ${TESTFILE} NAME_WE)
		set (TESTNAME test-${NAME}-${TEST})

		add_executable (${TESTNAME} EXCLUDE_FROM_ALL ${TESTFILE})
		target_link_libraries (${TESTNAME} test-common ${TESTLIBS})
		add_test (${NAME}-${TEST} ${TESTNAME})
		
		list (APPEND ALL_TEST_EXES ${TESTNAME})
		set (ALL_TEST_EXES ${ALL_TEST_EXES} PARENT_SCOPE)
	endforeach (TESTFILE)
endfunction (add_tests)

add_tests (core "core/*.cc")
add_tests (interp "interp/*.cc")
add_tests (util "util/*.cc")

#message (${ALL_TEST_EXES})

add_custom_target (check COMMAND ${CMAKE_CTEST_COMMAND} DEPENDS ${ALL_TEST_EXES})


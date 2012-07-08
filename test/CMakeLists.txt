add_executable (evan-test main.cc)
target_link_libraries (evan-test core interp util)

enable_testing()
add_test (evan-test evan-test)
add_custom_target (check COMMAND ${CMAKE_CTEST_COMMAND} DEPENDS evan-test)


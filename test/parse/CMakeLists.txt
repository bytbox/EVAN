file (GLOB_RECURSE PARSETESTS "*.evan")
add_executable (evan-test-parse EXCLUDE_FROM_ALL test-parse.cc)
target_link_libraries (evan-test-parse parse core util)

add_custom_target (check-parse COMMAND evan-test-parse ${PARSETESTS} DEPENDS evan-test-parse COMMENT "Running evan-test-parse" VERBATIM)


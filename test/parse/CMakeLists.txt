file (GLOB_RECURSE PARSETESTS "*.evan")
add_executable (evan-test-parse EXCLUDE_FROM_ALL test-parse.cc)
target_link_libraries (evan-test-parse core util parse)

add_custom_target (check-parse COMMAND evan-test-parse ${PARSETESTS} DEPENDS evan-test-parse COMMENT "Running evan-test-parse" VERBATIM)


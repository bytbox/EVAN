add_executable (evanc evanc.cc)
target_link_libraries (evanc core compile typecheck parse util)

add_executable (evani evani.cc)
target_link_libraries (evani core interp parse util)

add_executable (evan-doc evan-doc.cc)
target_link_libraries (evan-doc core parse util)


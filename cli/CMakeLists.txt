add_executable (evanc evanc.cc)
target_link_libraries (evanc core compile util)

add_executable (evani evani.cc)
target_link_libraries (evani core interp util)


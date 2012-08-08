add_executable (evanc evanc.cc)
target_link_libraries (evanc parse compile typecheck core util)

add_executable (evani evani.cc)
target_link_libraries (evani parse core interp util foreign)

add_executable (evan-doc evan-doc.cc)
target_link_libraries (evan-doc core parse util)


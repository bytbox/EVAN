add_subdirectory (unit)
add_subdirectory (parse)

add_custom_target (check)
add_dependencies (check check-unit check-parse)


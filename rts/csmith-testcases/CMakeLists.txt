cmake_minimum_required(VERSION 3.4.1)

add_subdirectory(csmith)

add_compile_options(-Wno-error)

add_subdirectory(testcases_tier1)
add_subdirectory(testcases_tier2)

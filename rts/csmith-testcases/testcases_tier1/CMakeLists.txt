cmake_minimum_required(VERSION 3.4.1)

include(${PROJECT_SOURCE_DIR}/FinishBuilding.cmake)

link_libraries(csmith)

add_custom_target(csmith_tier1_tests)

FILE(GLOB Sources "*.c")

foreach(s ${Sources})

Finish("${s}")

get_filename_component(test ${s} NAME_WE)
add_dependencies(csmith_tier1_tests ${test})

endforeach()

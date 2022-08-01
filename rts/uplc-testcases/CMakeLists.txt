cmake_minimum_required(VERSION 3.4.1)

include(${PROJECT_SOURCE_DIR}/FinishBuilding.cmake)

FILE(GLOB Sources "*.c")

foreach(s ${Sources})

Finish("${s}")

endforeach()

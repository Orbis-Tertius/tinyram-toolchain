cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources "*.plc.c")

foreach(s ${Sources})
  get_filename_component(test ${s} NAME_WE)
  add_executable(${test} ${s})
  target_link_libraries(${test} rts)

  if (${CMAKE_CROSSCOMPILING})
    target_link_options(${test} PUBLIC "-T${CMAKE_SOURCE_DIR}/tinyRAM.ld")
    target_link_libraries(${test} bootstrap)
  endif()
endforeach()


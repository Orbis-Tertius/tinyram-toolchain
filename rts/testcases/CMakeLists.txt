cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources "*.plc.c")

foreach(s ${Sources})
  get_filename_component(lib_we ${s} NAME_WE)
  add_executable(${lib_we}.bin ${s})
  target_link_libraries(${lib_we}.bin uplc2c_rts bootstrap)

  target_link_options(${lib_we}.bin PUBLIC "-T${CMAKE_SOURCE_DIR}/tinyRAM.ld")
  #set_target_properties(${lib_we}.bin PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/tinyRAM.ld)
endforeach()


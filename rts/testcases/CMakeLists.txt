cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources "*.plc.c")

foreach(s ${Sources})
  get_filename_component(lib_we ${s} NAME_WE)
  add_executable(${lib_we}.bin ${s})
  target_link_libraries(${lib_we}.bin uplc2c_rts)
endforeach()


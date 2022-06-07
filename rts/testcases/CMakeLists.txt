cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources "*.plc.c")

foreach(s ${Sources})

	enable_language(ASM)
  get_filename_component(test ${s} NAME_WE)
  add_executable(${test} ${s})
  target_link_libraries(${test} rts)

  if (${CMAKE_CROSSCOMPILING})
    target_link_options(${test} PUBLIC "-T${CMAKE_SOURCE_DIR}/tinyRAM.ld")
    #target_link_libraries(${test} bootstrap)

    target_sources(${test} PUBLIC
          ../bootstrap/preamble.S
          ../bootstrap/start.c
          ../bootstrap/special.c
          ../bootstrap/builtins.c
          ../bootstrap/memcpy.c
          )

    add_custom_command(
      TARGET ${test}
      POST_BUILD
      COMMAND ${CMAKE_OBJCOPY} ARGS -O binary ${CMAKE_CURRENT_BINARY_DIR}/${test} ${CMAKE_CURRENT_BINARY_DIR}/${test}.bin
    )
  endif()
endforeach()


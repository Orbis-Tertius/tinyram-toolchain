cmake_minimum_required(VERSION 3.4.1)

macro(Finish s)

  if (${CMAKE_CROSSCOMPILING})
    get_filename_component(test ${s} NAME_WE)
    add_executable(${test} ${s})

    target_link_libraries(${test} PUBLIC rts)
    target_link_libraries(${test} PUBLIC -whole-archive bootstrap)
    target_link_libraries(${test} PUBLIC gmp)

    target_link_options(${test} PUBLIC "-T${CMAKE_SOURCE_DIR}/tinyRAM.ld")

    set(BINARY "${CMAKE_CURRENT_BINARY_DIR}/${test}.bin")

    add_custom_command(
      OUTPUT ${BINARY}
      COMMAND ${PROJECT_SOURCE_DIR}/finish-building.sh ${BINARY} $<TARGET_FILE:${test}>
      DEPENDS ${test}
      COMMENT "Generate .bin for ${test}"
      VERBATIM
    )

    add_custom_target(${test}.bin ALL DEPENDS ${BINARY})
  else()
    get_filename_component(test ${s} NAME_WE)
    add_executable(${test} ${s})
    target_link_libraries(${test} rts)
  endif()

endmacro()


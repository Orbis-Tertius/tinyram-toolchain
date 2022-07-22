cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources "*.plc.c")

foreach(s ${Sources})

  if (${CMAKE_CROSSCOMPILING})
    get_filename_component(test ${s} NAME_WE)
    add_library(${test} ${s})

    target_link_libraries(${test} PUBLIC rts)
    target_link_libraries(${test} PUBLIC -whole-archive bootstrap)

    set(BINARY "${CMAKE_CURRENT_BINARY_DIR}/${test}.o")

    add_custom_command(
      OUTPUT ${BINARY}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/finish-building.sh ${BINARY} ${PROJECT_SOURCE_DIR} $<TARGET_FILE:gmp> $<TARGET_FILE:rts> $<TARGET_FILE:bootstrap> $<TARGET_FILE:${test}>
      DEPENDS gmp rts bootstrap ${test}
      COMMENT "Manually finish building ${test}"
      VERBATIM
    )

    add_custom_target(${test}.finish ALL DEPENDS ${BINARY})
  else()
    get_filename_component(test ${s} NAME_WE)
    add_executable(${test} ${s})
    target_link_libraries(${test} rts)
  endif()
endforeach()


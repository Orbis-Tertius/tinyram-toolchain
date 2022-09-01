cmake_minimum_required(VERSION 3.4.1)

project (UPLC2C_RTS)

enable_language(ASM)

set(CMAKE_EXPORT_COMPILE_COMMANDS "ON")

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wall -ffunction-sections")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -Wall -ffunction-sections")

if (${CMAKE_CROSSCOMPILING})
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --gc-sections")

  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -nostdinc -Qunused-arguments")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -nostdinc -Qunused-arguments")
  include_directories(SYSTEM tinyRAM_include)

  add_subdirectory(bootstrap)
endif()

add_subdirectory(rts)
add_subdirectory(gmp)

add_subdirectory(uplc-testcases)
add_subdirectory(csmith-testcases)

if (NOT ${CMAKE_CROSSCOMPILING})
  add_subdirectory(bin)
endif()

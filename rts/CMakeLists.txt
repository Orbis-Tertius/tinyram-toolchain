cmake_minimum_required(VERSION 3.4.1)

project (UPLC2C_RTS)

set(CMAKE_EXPORT_COMPILE_COMMANDS "ON")

add_compile_definitions(MINI_GMP_DONT_USE_FLOAT_H)

include_directories(${CMAKE_SOURCE_DIR})

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -Wall")

FILE(GLOB Sources *.c bytestring/*.c string/*.c data/*.c integer/*.c list/*.c misc/*.c pair/*.c)

add_library(uplc2c_rts
	    STATIC
	    ${Sources}
	    mini-gmp.c
	    print.cpp)

target_include_directories(uplc2c_rts PUBLIC .)

if (${CMAKE_CROSSCOMPILING})
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -nostdinc -Qunused-arguments")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -nostdinc -Qunused-arguments")
  include_directories(SYSTEM tinyRAM_include)

  add_subdirectory(bootstrap)
endif()

add_subdirectory(testcases)


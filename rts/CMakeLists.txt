cmake_minimum_required(VERSION 3.4.1)

project (UPLC2C_RTS)

set(CMAKE_EXPORT_COMPILE_COMMANDS "ON")

include_directories(${CMAKE_SOURCE_DIR})

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -g -O -Wall")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror -g -O -Wall")

FILE(GLOB Sources *.c bytestring/*.c string/*.c data/*.c integer/*.c list/*.c misc/*.c pair/*.c)

add_library(uplc2c_rts
	    STATIC
            ${Sources}
	    mini-gmp.c
	    print.cpp)

target_include_directories(uplc2c_rts PUBLIC .)

add_subdirectory(testcases)


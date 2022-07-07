cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources *.c bytestring/*.c string/*.c data/*.c integer/*.c list/*.c misc/*.c pair/*.c)

add_library(rts
	    STATIC
	    ${Sources}
	    print.cpp)

target_link_libraries(rts gmp)

target_include_directories(rts PUBLIC .)

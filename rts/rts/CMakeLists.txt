cmake_minimum_required(VERSION 3.4.1)

FILE(GLOB Sources *.c bytestring/*.c string/*.c data/*.c integer/*.c list/*.c misc/*.c pair/*.c)

add_library(rts
	    STATIC
	    ${Sources}
	    deserialize_data.c
	    print.c
	    apply_script_args.c)

if (NOT ${CMAKE_CROSSCOMPILING})
  target_sources(rts PRIVATE
    arch/x86/input_tapes.cpp
    arch/x86/putC.c
    arch/x86/util.cpp)
else()
  target_sources(rts PRIVATE
    arch/TinyRAM/input_tapes.c
    arch/TinyRAM/putC.S)
endif()

target_link_libraries(rts gmp)

target_include_directories(rts PUBLIC .)


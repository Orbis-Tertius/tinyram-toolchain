cmake_minimum_required(VERSION 3.4.1)

enable_language(ASM)

FILE(GLOB Sources *.c)

add_library(bootstrap
	    STATIC
	    ${Sources}
	    preamble.S
	    const_init.S)

target_include_directories(bootstrap PRIVATE .)

add_compile_options("-nostdinc -Qunused-arguments")

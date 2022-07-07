cmake_minimum_required(VERSION 3.4.1)

add_compile_definitions(MINI_GMP_DONT_USE_FLOAT_H)

add_library(gmp
	    STATIC
	    mini-gmp.c)

target_include_directories(gmp PUBLIC .)


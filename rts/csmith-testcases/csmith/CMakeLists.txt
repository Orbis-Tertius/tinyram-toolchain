cmake_minimum_required(VERSION 3.4.1)

add_library(csmith INTERFACE)

target_include_directories(csmith INTERFACE .)

if (${CMAKE_CROSSCOMPILING})
  target_include_directories(csmith INTERFACE TinyRAM)
else()
  target_include_directories(csmith INTERFACE X86)
endif()

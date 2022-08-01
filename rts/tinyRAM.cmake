set(CMAKE_SYSTEM_NAME               Generic)

# Without that flag CMake is not able to pass test compilation check
set(CMAKE_TRY_COMPILE_TARGET_TYPE   STATIC_LIBRARY)

set(CMAKE_AR                        llvm-ar)
set(CMAKE_ASM_COMPILER              clang)
set(CMAKE_C_COMPILER                clang)
set(CMAKE_CXX_COMPILER              clang++)

set(CMAKE_CUSTOM_LINKER             ld.lld)
set(CMAKE_C_LINK_EXECUTABLE
	"${CMAKE_CUSTOM_LINKER} --entry main <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set(CMAKE_CXX_LINK_EXECUTABLE
	"${CMAKE_CUSTOM_LINKER} --entry main <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set(CMAKE_OBJCOPY                   llvm-objcopy)
set(CMAKE_RANLIB                    llvm-ranlib)
set(CMAKE_SIZE                      llvm-size)
set(CMAKE_STRIP                     llvm-strip)

set(CMAKE_C_FLAGS                   "-target tinyRAM -std=c11 -mllvm -enable-tail-merge=false")
set(CMAKE_CXX_FLAGS                 "-target tinyRAM -mllvm -enable-tail-merge=false")
set(CMAKE_ASM_FLAGS                 "-target tinyRAM")

set(CMAKE_ASM_FLAGS_DEBUG           "-O1" CACHE INTERNAL "")
set(CMAKE_ASM_FLAGS_RELEASE         "-O3 -DNDEBUG" CACHE INTERNAL "")
set(CMAKE_C_FLAGS_DEBUG             "-O1" CACHE INTERNAL "")
set(CMAKE_C_FLAGS_RELEASE           "-O3 -DNDEBUG" CACHE INTERNAL "")
set(CMAKE_CXX_FLAGS_DEBUG           "${CMAKE_C_FLAGS_DEBUG}" CACHE INTERNAL "")
set(CMAKE_CXX_FLAGS_RELEASE         "${CMAKE_C_FLAGS_RELEASE}" CACHE INTERNAL "")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

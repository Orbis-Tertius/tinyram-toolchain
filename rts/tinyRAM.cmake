set(CMAKE_SYSTEM_NAME               Generic)
set(CMAKE_SYSTEM_PROCESSOR          tinyRAM)

# Without that flag CMake is not able to pass test compilation check
set(CMAKE_TRY_COMPILE_TARGET_TYPE   STATIC_LIBRARY)

set(TINYRAM_TOOLCHAIN_PATH "/home/marcin/projects/llvm-project/build/bin/")

set(CMAKE_AR                        ${TINYRAM_TOOLCHAIN_PATH}llvm-ar)
set(CMAKE_ASM_COMPILER              ${TINYRAM_TOOLCHAIN_PATH}clang)
set(CMAKE_C_COMPILER                ${TINYRAM_TOOLCHAIN_PATH}clang)
set(CMAKE_CXX_COMPILER              ${TINYRAM_TOOLCHAIN_PATH}clang++)

set(CMAKE_CUSTOM_LINKER             ${TINYRAM_TOOLCHAIN_PATH}ld.lld)
set(CMAKE_LINKER                    ${TINYRAM_TOOLCHAIN_PATH}ld.lld)
set(CMAKE_C_LINK_EXECUTABLE
    "/home/marcin/projects/llvm-project/build/bin/ld.lld <CMAKE_C_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set(CMAKE_CXX_LINK_EXECUTABLE
    "/home/marcin/projects/llvm-project/build/bin/ld.lld  <CMAKE_CXX_LINK_FLAGS> <LINK_FLAGS> <OBJECTS> -o <TARGET> <LINK_LIBRARIES>")
set(CMAKE_OBJCOPY                   ${TINYRAM_TOOLCHAIN_PATH}llvm-objcopy)
set(CMAKE_RANLIB                    ${TINYRAM_TOOLCHAIN_PATH}llvm-ranlib)
set(CMAKE_SIZE                      ${TINYRAM_TOOLCHAIN_PATH}llvm-size)
set(CMAKE_STRIP                     ${TINYRAM_TOOLCHAIN_PATH}llvm-strip)

#set(CMAKE_C_FLAGS                   "-Wno-psabi --specs=nosys.specs -fdata-sections -ffunction-sections -Wl,--gc-sections" CACHE INTERNAL "")
#set(CMAKE_CXX_FLAGS                 "${CMAKE_C_FLAGS} -fno-exceptions" CACHE INTERNAL "")

set(CMAKE_C_FLAGS                   "-target tinyRAM -std=c11 -mllvm -enable-tail-merge=false")
set(CMAKE_CXX_FLAGS                 "-target tinyRAM -mllvm -enable-tail-merge=false")
set(CMAKE_ASM_FLAGS                 "-target tinyRAM")

set(CMAKE_ASM_FLAGS_DEBUG           "-O0" CACHE INTERNAL "")
set(CMAKE_ASM_FLAGS_RELEASE         "-O3 -DNDEBUG" CACHE INTERNAL "")
set(CMAKE_C_FLAGS_DEBUG             "-O0" CACHE INTERNAL "")
set(CMAKE_C_FLAGS_RELEASE           "-O3 -DNDEBUG" CACHE INTERNAL "")
set(CMAKE_CXX_FLAGS_DEBUG           "${CMAKE_C_FLAGS_DEBUG}" CACHE INTERNAL "")
set(CMAKE_CXX_FLAGS_RELEASE         "${CMAKE_C_FLAGS_RELEASE}" CACHE INTERNAL "")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

#!/bin/bash

set -e

base_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

rts_src_dir="$base_dir/rts"
compiler_src_dir="$base_dir/compiler"

cd $rts_src_dir/uplc-testcases
./compile_testcase.sh


targets="gmp rts test_validator_1 test_validator_2  test_validator_3"

# x86
x86_rts_build_dir=$(mktemp -d)
cd $x86_rts_build_dir

cmake $rts_src_dir
make $targets deserialize

# TinyRAM
tinyram_rts_build_dir=$(mktemp -d)
cd $tinyram_rts_build_dir

# -DCMAKE_BUILD_TYPE=Debug
cmake -DCMAKE_TOOLCHAIN_FILE=$rts_src_dir/tinyRAM.cmake $rts_src_dir
make $targets test_validator_1.bin test_validator_2.bin test_validator_3.bin


export UPLC2C_INC_DIR=$rts_src_dir
export UPLC2C_X86_LIB_DIR=$x86_rts_build_dir
export UPLC2C_TINYRAM_LIB_DIR=$tinyram_rts_build_dir

cd $compiler_src_dir
cabal test
status=$?

rm -r $x86_rts_build_dir
rm -r $tinyram_rts_build_dir

exit $status

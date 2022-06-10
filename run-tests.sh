#!/bin/bash

base_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

rts_src_dir="$base_dir/rts"
compiler_src_dir="$base_dir/compiler"

rts_build_dir=$(mktemp -d)
cd $rts_build_dir

cmake $rts_src_dir
make gmp rts

export UPLC2C_INC_DIR=$rts_src_dir
export UPLC2C_LIB_DIR=$rts_build_dir

cd $compiler_src_dir
cabal test
status=$?

rm -r $rts_build_dir

exit $status

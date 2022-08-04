#!/bin/bash

set -e

base_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cwd=`pwd`

# Build x86

x86_dir=$(mktemp -d)
cd $x86_dir
cmake $base_dir/rts
make csmith_tier1_tests csmith_tier2_tests

# Build TinyRAM Release

tinyram_release_dir=$(mktemp -d)
cd $tinyram_release_dir
cmake $base_dir/rts \
  -DCMAKE_TOOLCHAIN_FILE=$base_dir/rts/tinyRAM.cmake \
  -DCMAKE_BUILD_TYPE=Release
make csmith_tier1_tests csmith_tier2_tests

# Build TinyRAM Release

tinyram_debug_dir=$(mktemp -d)
cd $tinyram_debug_dir
cmake $base_dir/rts \
  -DCMAKE_TOOLCHAIN_FILE=$base_dir/rts/tinyRAM.cmake \
  -DCMAKE_BUILD_TYPE=Debug
make csmith_tier1_tests csmith_tier2_tests

# Execute and compare results

echo "Executing..."

touch empty
emulator_opts="-w 32 -r 16 --max-steps 2000000"
extract_hash="grep \"Answer: \""

for tier in {1..2};
do
  for number in {1..100};
  do
    rel="csmith-testcases/testcases_tier${tier}/csmith_tier${tier}_test_${number}"
    x86_path="$x86_dir/$rel"
    tinyram_release_path="$tinyram_release_dir/$rel"
    tinyram_debug_path="$tinyram_debug_dir/$rel"

    if [ ! -f "$x86_path" ]; then
      continue
    fi

    x86_hash=$($x86_path | eval $extract_hash)
    tinyram_release_hash=$(tinyram $emulator_opts "${tinyram_release_path}.bin" empty empty | eval $extract_hash)
    tinyram_debug_hash=$(tinyram $emulator_opts "${tinyram_debug_path}.bin" empty empty | eval $extract_hash)

    if [ "$x86_hash" == "$tinyram_debug_hash" ] && \
       [ "$tinyram_debug_hash" == "$tinyram_release_hash" ]
    then
      echo "TEST ${tier}/${number}  PASSED with: $x86_hash"
    else
      echo "!! TEST FAILED !!"
      echo "$x86_hash, $tinyram_debug_hash, $tinyram_release_hash"
      exit 1
    fi

  done
done

cd $cwd

rm -r $x86_dir
rm -r $tinyram_release_dir
rm -r $tinyram_debug_dir

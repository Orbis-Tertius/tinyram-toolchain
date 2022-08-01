#!/bin/bash

set -e

if [ "$#" -ne 2 ]; then
  echo "Usage: finish-building.sh DEST_FILE_PATH SRC_FILE_PATH"
  exit 1
fi

echo "Finishing building: $@"

dest=$1
src=$2

dir=$(mktemp -d -t ci-XXXXXXXXXX)

#llvm-objdump --triple=tinyRAM -Drt $src | less
#llvm-objdump --triple=tinyRAM -h --show-lma $src

echo "Align sections to 8 bytes"

sections=$(readelf -S $src | awk '{print $3}' | grep -oE '^(\.data|\.rodata)[^ ]*$' || true)
for s in $sections
do
  llvm-objcopy --set-section-alignment $s=8 $src
done

echo "Codegen"

codegen $src $dir/generated.s

clang -target tinyRAM -o $dir/generated -c $dir/generated.s

echo "Objcopy"

llvm-objcopy --only-section=.text -O binary $src $dest
llvm-objcopy --only-section=.text -O binary $dir/generated $dir/generated.bin

cat $dir/generated.bin >> $dest

rm -r $dir


#!/bin/bash

set -e

if [ "$#" -le 3 ]; then
  echo "Usage: finish-building.sh DEST_FILE_PATH PROJECT_SRC_DIR [ARCHIVE_FILE_PATH ...]"
  exit 1
fi

echo "Finishing building: $@"

dest=$1
project=$2

dir=$(mktemp -d -t ci-XXXXXXXXXX)

echo "Merge objects in archive into one ELF"

for i in $(seq 3 $#);
do
  mkdir $dir/$i
  cd $dir/$i
  eval archive=\$$i
  ar x $archive
  ld.lld --relocatable $(ls $dir/$i) -o $dir/$i.o
  sections=$(readelf -S $dir/$i.o | awk '{print $2}' | grep -oE '^(\.data|\.rodata)[^ ]*$' || true)
  for s in $sections
  do
    llvm-objcopy --set-section-alignment $s=8 $dir/$i.o
  done
done

list=""
for i in $(seq 3 $#);
do
  list="$list \$dir/$i.o"
done

eval ld.lld --relocatable $list -o $dir/merged.o

echo "Create and compile startup code that initializes .data and .rodata"

cd $project/../compiler/
cabal run codegen $dir/merged.o $dir/generated.s

clang -target tinyRAM -o $dir/generated.o -c $dir/generated.s

echo "Make global symbols at the beginning of a section"

toGlobalize=$(cat $dir/generated.s | grep '// init-symbol:' | awk "{ print \"llvm-objcopy --add-symbol \" \$3 \"=\" \$4 \":\" \$5 \",global,object\", \"$dir/merged.o\" }")
echo -e "$toGlobalize" > $dir/globalize
chmod u+x $dir/globalize
$dir/globalize

echo "Finally link an executable"

ld.lld -T${project}/tinyRAM.ld \
	--gc-sections \
	--entry main \
	-o $dest \
        $dir/merged.o \
	$dir/generated.o

#llvm-objdump --triple=tinyRAM -Drt $dest | less

echo "Objcopy"

llvm-objcopy -O binary $dest $dest.bin

echo "Cleanup"

rm -r $dir


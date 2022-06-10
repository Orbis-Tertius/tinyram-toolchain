#!/bin/bash

set -e

for tc in *.plc
do
  path="$(realpath $tc)"
  echo "### Building $path"
  (cd ../../compiler; cabal run uplc2c -- --plc $path "../rts/testcases/$tc.c")
done


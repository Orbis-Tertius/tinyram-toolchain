#!/bin/bash

set -e

for tc in test_standalone_*.plc
do
  path="$(realpath $tc)"
  echo "### Building $path"
  (cd ../../compiler; cabal run uplc2c -- --plc $path "../rts/uplc-testcases/$tc.c")
done

for tc in test_validator_*.plc
do
  path="$(realpath $tc)"
  echo "### Building $path"
  (cd ../../compiler; cabal run uplc2c -- --plc --validator $path "../rts/uplc-testcases/$tc.c")
done


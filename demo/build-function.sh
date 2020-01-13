#!/bin/bash

# Package a binary into a Lambda function

VARIANT="$1"

mkdir -p .build
rm -f .build/${VARIANT}-function.zip

cp `stack --docker path --local-install-root`/bin/infernal-${VARIANT}-demo .build/bootstrap

pushd .build
  ls -lh bootstrap
  zip ${VARIANT}-function.zip bootstrap
  rm bootstrap
  unzip -l ${VARIANT}-function.zip
popd

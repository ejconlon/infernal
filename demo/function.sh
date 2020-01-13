#!/bin/bash

# Package a binary into a Lambda function

VARIANT="$1"

mkdir -p .build

cp `stack --docker path --local-install-root`/bin/infernal-${VARIANT}-demo .build/${VARIANT}-bootstrap

pushd .build
  ls -lh ${VARIANT}-bootstrap
  zip ${VARIANT}-function.zip ${VARIANT}-bootstrap
  rm ${VARIANT}-bootstrap
  unzip -l ${VARIANT}-function.zip
popd

#!/bin/bash

set -eux

REPO_ROOT="${PWD}"

pushd /tmp
  rm -rf my-haskell-lambda
  stack new my-haskell-lambda ${REPO_ROOT}/stack-template.hsfiles
  pushd my-haskell-lambda
    make integration-test
  popd
popd

#!/bin/bash

# Test SAM invocations against expected responses

set -eu

cd "$(dirname "${BASH_SOURCE[0]}")"

VARIANT="$1"
FUNCTION="$2"

if [ ! -f ../.build/${VARIANT}-function.zip ]; then
  echo "Please run `make build && build-function.sh ${VARIANT}`."
  exit 1
fi

for CASE in `ls ${VARIANT}`
do
  echo "Testing case ${CASE}:"
  REQUEST="${VARIANT}/${CASE}/request.json"
  EXPECTED="${VARIANT}/${CASE}/response.json"
  ACTUAL="/tmp/${VARIANT}-${CASE}-response.json"
  sam local invoke ${FUNCTION} --event ${REQUEST} > ${ACTUAL}
  # Ensure there's a newline at the end of the file
  awk '/^$/{f=1}END{ if (!f) {print "\n"}}1' ${ACTUAL}
  echo "" >> ${ACTUAL}
  diff <(tail -n1 ${ACTUAL}) <(cat ${EXPECTED})
  echo "=============================="
done

echo "All tests passed."

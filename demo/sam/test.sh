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

for CASE in ok bad throw
do
  echo "Testing case ${CASE}:"
  EVENT="events/${CASE}-request.json"
  EXPECTED="expected/${CASE}-response.json"
  ACTUAL="/tmp/${CASE}-repsonse.json"
  sam local invoke ${FUNCTION} --event ${EVENT} > ${ACTUAL}
  diff <(tail -n1 ${ACTUAL}) <(cat ${EXPECTED})
  echo "=============================="
done

echo "All tests passed."

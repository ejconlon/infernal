#!/bin/bash

# Tests

set -eu

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ ! -f ../infernal-demo/.build/function.zip ]; then
  echo "Please 'make build' infernal-demo first."
  exit 1
fi

for CASE in ok bad throw
do
  echo "Testing case ${CASE}:"
  EVENT="events/${CASE}-request.json"
  EXPECTED="expected/${CASE}-response.json"
  ACTUAL="/tmp/${CASE}-repsonse.json"
  sam local invoke --event ${EVENT} > ${ACTUAL}
  diff <(tail -n1 ${ACTUAL}) <(cat ${EXPECTED})
  echo "=============================="
done

echo "All tests passed."

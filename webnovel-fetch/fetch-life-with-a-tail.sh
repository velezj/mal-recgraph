#!/usr/bin/env bash

ROOT_URL="https://re-library.com/translations/life-with-a-tail/"
OUTPUT_DIR="life-with-a-tail/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/life-with-a-tail[^?]*/$" "${ROOT_URL}" | tee "${LOGFILE}"

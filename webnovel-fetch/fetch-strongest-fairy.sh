#!/usr/bin/env bash

ROOT_URL="https://yurikatrans.xyz/the-strongest-fairy-zz/"
OUTPUT_DIR="the-strongest-fairy/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/the-strongest-fairy/\d+/$" "${ROOT_URL}" | tee "${LOGFILE}"

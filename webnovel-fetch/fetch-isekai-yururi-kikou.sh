#!/usr/bin/env bash

ROOT_URL="https://novelonomicon.com/category/novels/isekai-yururi-kikou/"
OUTPUT_DIR="isekai-yururi-kikou/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 30 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/isekai-yururi-kikou/(page/\d+|[^/?]*chapter[^/?]+)/$" "${ROOT_URL}" | tee "${LOGFILE}"


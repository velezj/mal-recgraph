#!/usr/bin/env bash

ROOT_URL="https://novelonomicon.com/category/novels/nigotta-hitomi-no-lilianne/"
OUTPUT_DIR="nigotta-hitomi-no-lilianne/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/nigotta-hitomi-no-lilianne/(page/\d+|lil-chapter[^/?]+)/$" "${ROOT_URL}" | tee "${LOGFILE}"

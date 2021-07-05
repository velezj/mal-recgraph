#!/usr/bin/env bash

ROOT_URL="https://translations.craneanime.com/index/angel-little-sister/"
OUTPUT_DIR="angel-little-sister/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?angel-[^/?]*?[0-9]+/$" "${ROOT_URL}" | tee "${LOGFILE}"

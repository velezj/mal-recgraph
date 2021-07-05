#!/usr/bin/env bash

ROOT_URL="https://experimentaltranslations.com/grimoire-master-of-an-everchanging-world/"
OUTPUT_DIR="grimoire-master-of-an-everchanging-world/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?grimoire-master-[^/?]*?[0-9]+[^/?]*/$" "${ROOT_URL}" | tee "${LOGFILE}"

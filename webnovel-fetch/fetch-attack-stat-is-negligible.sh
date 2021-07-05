#!/usr/bin/env bash

ROOT_URL="https://www.scribblehub.com/read/293763-my-attack-stat-is-negligible-so-i-cant-help-but-rely-on-critical-attacks-to-succeed/chapter/293767/"
ROOT_URL="https://www.scribblehub.com/read/293763-my-attack-stat-is-negligible-so-i-cant-help-but-rely-on-critical-attacks-to-succeed/chapter/294453/"
OUTPUT_DIR="my-attack-stats-is-negligible/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive --directory-prefix "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^[^?]+/chapter/\\d+/" "${ROOT_URL}" | tee "${LOGFILE}"

#!/usr/bin/env bash

ROOT_URL="https://adrenalineforfuture.wordpress.com/2020/09/08/alice-san-wa-template-o-shiranai/"
OUTPUT_DIR="alice-san-wa-template-o-shiranai/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?alice-c-[0-9]+/$" "${ROOT_URL}" | tee "${LOGFILE}"

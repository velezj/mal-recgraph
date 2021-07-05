#!/usr/bin/env bash

ROOT_URL="https://novelsjapan.wordpress.com/some-random-novel-chapter-1/"
OUTPUT_DIR="reborn-as-a-vending-machine/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?chapter-[0-9]+[^/?]*/$" "${ROOT_URL}" | tee "${LOGFILE}"

ROOT_URL_B="https://honyakusite.wordpress.com/vending-machine/"
LOGFILE_B="${OUTPUT_DIR}/fetch_b.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?vendm[^/?]*[0-9]+[^/?]*/$" "${ROOT_URL_B}" | tee "${LOGFILE_B}"

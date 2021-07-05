#!/usr/bin/env bash

ROOT_URL="https://www.ihavesinnedtranslation.com/p/tensei-shite-inaka-de-slowlife-wo.html"
OUTPUT_DIR="tensei-shite-inaka-de-slowlife-wo-okuritai/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/[^/?]*?(tensei.*|slowlife.*|/[0-9]+/[0-9]+/\[0-9]+.html)$" "${ROOT_URL}" | tee "${LOGFILE}"

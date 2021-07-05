#!/usr/bin/env bash

ROOT_URL="https://travistranslations.com/novel/12399/i-was-a-sword-when-i-reincarnated#toc"
OUTPUT_DIR="reincarnated-as-a-sword/"
LOGFILE="${OUTPUT_DIR}/fetch.wget.log"
mkdir -p "${OUTPUT_DIR}"

wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?reincarnated[^/?]*?/chapter-[0-9]+$" "${ROOT_URL}" | tee "${LOGFILE}"


ROOT_URL_B="https://todstl.blogspot.com/p/tenken-table-of-content.html"
LOGFILE="${OUTPUT_DIR}/fetch-b.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 2 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?(tenken-|posts/[0-9]+)[^?]*$" "${ROOT_URL_B}" | tee "${LOGFILE}"


ROOT_URL_C="https://www.scribblehub.com/series/207976/tensei-shitara-ken-deshita/"
LOGFILE="${OUTPUT_DIR}/fetch-c.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?(chapter/[0-9]+/|[?]toc=.*)$" "${ROOT_URL_C}" | tee "${LOGFILE}"


ROOT_URL_D="https://savedaopress.wordpress.com/2021/04/07/sword-chapter-378-1-the-missing-romeo-i/"
LOGFILE="${OUTPUT_DIR}/fetch-d.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?sword-chapter[^?]*/$" "${ROOT_URL_D}" | tee "${LOGFILE}"



ROOT_URL_E="https://randomtranslations129321186.wordpress.com/2020/02/26/338-friendly-fire/"
LOGFILE="${OUTPUT_DIR}/fetch-e.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/[0-9]+[^?]*/$" "${ROOT_URL_E}" | tee "${LOGFILE}"



ROOT_URL_F="https://celestialmaid.wixsite.com/website/tskd"
LOGFILE="${OUTPUT_DIR}/fetch-f.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/tksd-chapter-[0-9]+$" "${ROOT_URL_F}" | tee "${LOGFILE}"



ROOT_URL_G="https://www.scribblehub.com/series/160889/i-was-a-sword-when-i-reincarnated-wn/"
LOGFILE="${OUTPUT_DIR}/fetch-g.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?(chapter/[0-9]+/|[?]toc=.*)$" "${ROOT_URL_G}" | tee "${LOGFILE}"



ROOT_URL_H="https://cardboardtranslations.com/tskd-304/"
LOGFILE="${OUTPUT_DIR}/fetch-h.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/tskd[^/?]*[0-9]+[^/?]/$" "${ROOT_URL_H}" | tee "${LOGFILE}"



ROOT_URL_I="https://comettranslations.weebly.com/i-was-a-sword-when-i-reincarnated.html"
LOGFILE="${OUTPUT_DIR}/fetch-i.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*?/i-was-a-sword-when-i-reincarnated-chapter[^/?]*/$" "${ROOT_URL_I}" | tee "${LOGFILE}"



ROOT_URL_J="https://ensigblog.wordpress.com/current-works-main/i-was-a-sword-when-i-reincarnated-main/"
LOGFILE="${OUTPUT_DIR}/fetch-j.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*chapter[^?]*/$" "${ROOT_URL_J}" | tee "${LOGFILE}"


ROOT_URL_K="https://pengutaichou.wordpress.com/2017/03/22/sword-shisho-66/"
LOGFILE="${OUTPUT_DIR}/fetch-k.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^.*sword-shisho[^?]*/$" "${ROOT_URL_K}" | tee "${LOGFILE}"



ROOT_URL_L="https://kisatohobbies.wordpress.com/2017/01/23/58/"
LOGFILE="${OUTPUT_DIR}/fetch-l.wget.log"
wget --verbose --recursive -P "${OUTPUT_DIR}" --wait 20 --random-wait --trust-server-names -l 1000 --convert-links --page-requisites --regex-type "pcre" --accept-regex "^L$" "${ROOT_URL_L}" | tee "${LOGFILE}"



#!/usr/bin/env bash
set -x
set -e
set -u
set -o pipefail

INPUT="$1"
CHAPTER_NUM="$2"
SED_OUTPUT="${INPUT}_sed.html"
TIDY_OUTPUT="${SED_OUTPUT}_tidy.xhtml"
LINT_OUTPUT="${TIDY_OUTPUT}_xmllint.xhtml"
PROC_OUTPUT="${LINT_OUTPUT}_xsltprocessed.xhtml"
CHUNK_OUTPUT="${PROC_OUTPUT}_chunked.chunk"

cp "$INPUT" "$SED_OUTPUT"
# sed -i 's/<argu/ < argu/g' "$SED_OUTPUT"
# sed -i 's/<t.l/ < t.l/g' "$SED_OUTPUT"
# sed -i 's/<script>\(.*?\)<\/script>/<script\/>/g' "$SED_OUTPUT"
sed -i '/<script/,/<\/script>/d' "$SED_OUTPUT"
sed -i -n '/TOC<\/span>/,/TOC<\/span>/p' "$SED_OUTPUT"
echo "<!DOCTYPE html><html><head><title>Chapter $CHAPTER_NUM </title></head><body><div class=\"entry-content\"><h1 class=\"chapter\">Chapter $CHAPTER_NUM </h1><p>" > tmpfile && cat "$SED_OUTPUT" >> tmpfile && mv tmpfile "$SED_OUTPUT"
#echo "</p></p></div></body></html>" >> "$SED_OUTPUT"
tidy -xml -output "$TIDY_OUTPUT" "$SED_OUTPUT"
xmllint --dropdtd --noblanks --nocatalogs --nocdata --nonet --nsclean --xmlout --format "$TIDY_OUTPUT" > "$LINT_OUTPUT"
xsltproc --output "$PROC_OUTPUT" transform.xslt "$LINT_OUTPUT"
tail -n +2 "$PROC_OUTPUT" > "$CHUNK_OUTPUT"
cat "$CHUNK_OUTPUT"

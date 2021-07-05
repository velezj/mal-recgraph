#!/usr/bin/env fish

set BASE_PATH "/home/velezj/projects/personal/repos/mal-recgraph/webnovel-fetch/isekai-yururi-kikou/novelonomicon.com/novels/isekai-yururi-kikou"
set OUTPUT_FILE "all-merged.chunk.html"

rm -rf "$OUTPUT_FILE"
for i in (seq 1 293)
    if ./process-file.sh "$BASE_PATH/chapter-$i/index.html" "$i" >> "$OUTPUT_FILE"
	echo "woot"
    else
	exit 255
    end
end

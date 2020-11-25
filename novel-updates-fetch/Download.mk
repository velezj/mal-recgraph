SHELL := /bin/bash
WGET_WAIT = 10
WGET_LIMIT_RATE = 900k


all: $(addsuffix .touch,$(subst links_,finished_,$(wildcard links_*)))


finished_%.touch: links_%
	-name=$(subst links_,,$<) ;\
	dirname=chapters_$${name} ;\
	urlsname=urls_$${name}.txt ;\
	logfile=log.txt ;\
	echo "-----------------------------------------------------" ;\
	echo "Starting $${name}" ;\
	echo "-----------------------------------------------------" ;\
	mkdir -p "$${dirname}" ;\
	cd "$${dirname}" ;\
	cat ../$< | tr --delete '"' > "$${urlsname}" ;\
	wget --wait=${WGET_WAIT} --random-wait --input-file="$${urlsname}" --verbose
	touch "${@}"


links_%.get: finished_%.touch
	echo "fetched"


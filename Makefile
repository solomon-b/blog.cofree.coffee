ORG = $(wildcard org/*.org)
MD = $(subst org/,markdown/,$(addsuffix .md,$(basename $(ORG))))
HTML = $(subst org/,html/,$(addsuffix /index.html,$(basename $(ORG))))

markdown/%.md: org/%.org
	pandoc -s $< -t markdown -o $@

html/%/index.html: org/%.org
	mkdir -p $(dir $@)
	pandoc --template=./html-template/template.html -s $< -t html -o "${@}"

default: $(MD) $(HTML)
	./build-index.sh

publish:
	git push publish

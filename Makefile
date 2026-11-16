ORG = $(wildcard org/*.org)
MD = $(subst org/,markdown/,$(addsuffix .md,$(basename $(ORG))))
HTML = $(subst org/,html/,$(addsuffix /index.html,$(basename $(ORG))))

markdown/%.md: org/%.org
	pandoc --lua-filter=./filters/literate.lua -s $< -t markdown -o $@

html/%/index.html: org/%.org
	mkdir -p $(dir $@)
	pandoc --template=./html-template/template.html --lua-filter=./filters/literate.lua --lua-filter=./filters/highlight.lua -s $< -t html -o "${@}"

default: $(MD) $(HTML)
	./build-index.sh
	./build-rss.sh

# Every post takes part. A Haskell block with no ':tangle' target stays out.
LITERATE = $(ORG)

# All posts tangle into one source tree. A block names its module file with
# ':tangle Post/Module.hs', or takes the name from a file or subtree property.
# Org resolves a relative ':tangle' path against the org file, so tangle a copy.
#
# Happy and Alex blocks tangle to 'grammar/Post/Module.y' and '.x'. Each grammar
# directory is its own source root, because the posts name the modules 'Lexer'
# and 'Parser'.
#
# An Idris block tangles to 'idris1/Post/Module.idr' or 'idris2/Post/Module.idr'.
# The directory selects the compiler. The 'idris' label is Idris 1.
#
# A GHCi session block tangles to 'sessions/Module.Name.ghci', or to
# 'grammar/Post/Module.ghci'. 'check-sessions.sh' runs it against that module.
tangled/.tangled: $(LITERATE)
	rm -rf tangled
	mkdir -p tangled
	for f in $(LITERATE); do \
		cp $$f tangled/post.org && \
		emacs --batch --eval '(progn (require (quote ob-tangle)) (setq create-lockfiles nil) (push (quote (:mkdirp . "yes")) org-babel-default-header-args) (org-babel-tangle-file "tangled/post.org" nil "haskell\\|happy\\|alex\\|idris"))' || exit 1; \
	done
	rm tangled/post.org
	for f in $$(find tangled/grammar -name '*.x'); do alex $$f || exit 1; done
	for f in $$(find tangled/grammar -name '*.y'); do happy --ghc $$f || exit 1; done
	touch $@

check: tangled/.tangled
	ghc -fno-code -itangled $$(find tangled -name '*.hs' -not -path 'tangled/grammar/*')
	for d in tangled/grammar/*/; do ghc -fno-code -itangled -i$$d $$d*.hs || exit 1; done
	cd tangled/idris1 && for f in $$(find . -name '*.idr'); do idris --check $$f || exit 1; done
	cd tangled/idris2 && for f in $$(find . -name '*.idr'); do idris2 --check $$f || exit 1; done
	./check-sessions.sh

publish: check
	git push --force publish

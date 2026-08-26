.PHONY : build
build :
	dune build -p markup,markup-lwt

.PHONY : format
format :
	dune build @src/lite/fmt --auto-promote || dune build @src/lite/fmt

# This is not part of the ordinary build process. The output file, entities.ml,
# is checked into git.
.PHONY : entities
entities :
	dune exec src/entities/translate_entities/translate_entities.exe \
	  > src/entities/entities.ml

.PHONY : lite-ragel
lite-ragel :
	cd src/lite && ragel-ocaml -L -F1 \
	  -o ragel_html_tokenizer.ml ragel_html_tokenizer.ml.rl
	python3 -c 'from pathlib import Path; p = Path("src/lite/ragel_html_tokenizer.ml"); p.write_text("\n".join(line.rstrip() for line in p.read_text().splitlines()) + "\n")'
	rm -f src/lite/ragel_html_tokenizer.ri

.PHONY : test
test :
	dune runtest

LITE_TEST_EXE := _build/default/test/lite/lite_diff_corpus.exe
LITE_WRITER_TEST_EXE := _build/default/test/lite/lite_writer_diff_corpus.exe
LITE_TEST_CORPUS ?= big_tests

.PHONY : test-lite
test-lite :
	dune build --profile release test/lite/lite_diff_corpus.exe \
	  test/lite/lite_writer_diff_corpus.exe
	$(LITE_TEST_EXE) $(LITE_TEST_CORPUS)
	$(LITE_WRITER_TEST_EXE) $(LITE_TEST_CORPUS)

.PHONY : coverage
coverage :
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src/ --do-not-expect src/entities/translate_entities/
	bisect-ppx-report summary
	@echo See _coverage/index.html

.PHONY : performance-test
performance-test :
	dune exec test/performance/performance_markup.exe
	dune exec test/performance/performance_nethtml.exe
	dune exec test/performance/performance_xmlm.exe

BENCH_EXE := _build/default/test/performance/performance_corpus.exe
BENCH_CORPUS := big_tests/llm_tracker/

.PHONY : bench-release
bench-release :
	dune build --profile release test/performance/performance_corpus.exe

.PHONY : bench-hyperfine
bench-hyperfine : bench-release
	hyperfine '$(BENCH_EXE) $(BENCH_CORPUS)'

.PHONY : bench-perf
bench-perf : bench-release
	perf stat -r 5 $(BENCH_EXE) $(BENCH_CORPUS)

.PHONY : js-test
js-test :
	dune build test/js_of_ocaml/test_js_of_ocaml.bc.js

.PHONY : dependency-test
dependency-test :
	dune exec test/dependency/dep_core.exe
	dune exec test/dependency/dep_lwt.exe
	dune exec test/dependency/dep_lwt_unix.exe

# Everything from here to "clean" is inactive, pending porting to odoc.
OCAML_VERSION := \
	$(shell ocamlc -version | grep -E -o '^[0-9]+\.[0-9]+' | sed 's/\.//')

OCAMLBUILD := ocamlbuild -use-ocamlfind -j 0 -no-links

HTML := docs/html
DOCFLAGS := -docflags -colorize-code

if_package = ! ocamlfind query $(1) > /dev/null 2> /dev/null || ( $(2) )

.PHONY : docs
docs : docs-odocl
	$(OCAMLBUILD) $(DOCFLAGS) doc/$(LIB).docdir/index.html
	rm -rf $(HTML)
	mkdir -p $(HTML)
	rsync -r _build/doc/$(LIB).docdir/* $(HTML)/
	cp doc/style.css $(HTML)/
	$(call if_package,lambdasoup,\
	  test $(OCAML_VERSION) -eq 402 \
	  || ( make docs-postprocess \
	  && rm -f $(HTML)/type_*.html $(HTML)/html.stamp $(HTML)/index*.html \
	  && _build/doc/postprocess.native ))
	@echo "\nSee $(HTML)/index.html"

.PHONY : docs-postprocess
docs-postprocess :
	$(OCAMLBUILD) postprocess.native

ODOCL := doc/markup.odocl

.PHONY : docs-odocl
docs-odocl :
	echo Markup > $(ODOCL)
	$(call if_package,lwt,echo Markup_lwt >> $(ODOCL))
	$(call if_package,lwt.unix,echo Markup_lwt_unix >> $(ODOCL))

PUBLISH := docs/publish

.PHONY : publish-docs
publish-docs : check-doc-prereqs docs
	rm -rf $(PUBLISH)
	mkdir -p $(PUBLISH)
	cd $(PUBLISH) \
		&& git init \
		&& git remote add github git@github.com:aantron/markup.ml.git \
		&& rsync -r ../html/* ./ \
		&& git add -A \
		&& git commit -m 'Markup.ml documentation.' \
		&& git push -uf github master:gh-pages

DOC_ZIP := docs/$(LIB)-$(VERSION)-doc.zip

.PHONY : package-docs
package-docs : check-doc-prereqs docs
	rm -f $(DOC_ZIP)
	zip -9 $(DOC_ZIP) $(HTML)/*

.PHONY : check-doc-prereqs
check-doc-prereqs :
	@ocamlfind query lwt.unix > /dev/null 2> /dev/null \
		|| (echo "\nLwt not installed" && false)
	@ocamlfind query lambdasoup > /dev/null 2> /dev/null \
		|| (echo "\nLambda Soup not installed" && false)

.PHONY : clean
clean :
	rm -rf $(HTML) $(PUBLISH) $(DOC_ZIP)
	dune clean
	rm -rf _coverage

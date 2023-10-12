# DO NOT EDIT THIS FILE

.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

chat:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f ngrams.zip
	zip -r ngrams.zip . -x@exclude.lst

clean:
	dune clean
	rm -f ngrams.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
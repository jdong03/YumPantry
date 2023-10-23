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

yum:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f yum.zip
	zip -r yum.zip . -x@exclude.lst

clean:
	dune clean
	rm -f yum.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
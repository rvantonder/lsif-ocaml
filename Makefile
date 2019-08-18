all: build ocaml-lsif

build:
	dune build

ocaml-lsif:
	ln -s _build/install/default/bin/$@ ./$@

install:
	dune install

test:
	dune runtest

clean:
	dune clean

uninstall:
	dune uninstall

promote:
	dune promote

.PHONY: all build install test clean uninstall

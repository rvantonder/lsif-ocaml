all: build lsif-ocaml

build:
	dune build

lsif-ocaml:
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

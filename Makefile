all:
	dune build @install

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

.PHONY: all install test clean uninstall


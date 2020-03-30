.PHONY: all clean repl run build

all:
	dune exec bin/imp.bc

build:
	dune build bin/imp.bc
	dune build && dune install

clean:
	dune clean

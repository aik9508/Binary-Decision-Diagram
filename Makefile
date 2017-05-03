CB = corebuild -lib str

default: main
main: src/bddcycl.native
regex: src/regex.native
Tetravex: src/Tetravex.native

clean: 
	ocamlbuild -clean

%.native:
	$(CB) $@

.PHONY: default

.PHONY: top
top: main
	$(CB) src/bddcycl.cma
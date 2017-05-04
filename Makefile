CB = corebuild -lib str

default: main
main: src/bdd.native
regex: src/regex.native
tetravex: src/tetravex.native

clean: 
	ocamlbuild -clean

%.native:
	$(CB) $@

.PHONY: default

.PHONY: top
top: main
	$(CB) src/bddcycl.cma
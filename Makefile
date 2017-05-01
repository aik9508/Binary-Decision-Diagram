default: main
main: src/bddcycl.native
regex: src/regex.native
Tetravex: src/Tetravex.native

clean: 
	ocamlbuild -clean

%.native:
	corebuild -lib str $@

.PHONY: default
default: main
main: src/bddcycl.native
regex: src/regex.native

clean: 
	ocamlbuild -clean

%.native:
	corebuild -lib str $@

.PHONY: default
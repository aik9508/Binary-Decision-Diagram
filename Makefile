default: main
main: src/bddcycl.native

%.native:
	corebuild $@

.PHONY: default
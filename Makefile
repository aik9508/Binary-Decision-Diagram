CB_FLAGS = -lib str -I src
CB = corebuild $(CB_FLAGS)

all: lib bdd test

native: 
	$(CB) bdd.native

clean: 
	$(CB) -clean

lib: 
	$(CB) bdd_API.cma

test:
	$(CB) test.native

bdd: native
	./bdd.native satisfiable "a&&~a"

.PHONY: all clean lib native bdd
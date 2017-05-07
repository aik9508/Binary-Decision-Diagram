CB_FLAGS = -lib str -I src
CB = corebuild $(CB_FLAGS)

all: lib bdd

native: 
	$(CB) bdd.native

clean: 
	$(CB) -clean

lib: 
	$(CB) bdd_API.cma

bdd: native
	./bdd.native satisfiable "a&&~a"

.PHONY: all clean lib native bdd
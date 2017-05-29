CB_FLAGS = -lib str -I src
CB = corebuild $(CB_FLAGS)

all: bdd test

native: 
	$(CB) bdd.native

clean: 
	$(CB) -clean

test:
	$(CB) test.native

bdd: native
	./bdd.native satisfiable "a&&~a"

.PHONY: all clean native bdd
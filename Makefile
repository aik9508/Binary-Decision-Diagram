CB_FLAGS = -lib str -I src
CB = corebuild $(CB_FLAGS)

all: bdd test

bdd: 
	$(CB) bdd.native

clean: 
	$(CB) -clean

test:
	$(CB) test.native

.PHONY: all clean bdd
.PHONY: all test

APPS=Asteroids Counter CRUD CurrencyConverter NetMonitor TicTacToe TwoCounters Wave
OBJ=dist/build
COMPILE=ghc --make -i$(OBJ) -L/usr/lib -L$(OBJ) -isrc -i../reactive-banana/src -idist/build/autogen

all:
	cabal configure && cabal build --ghc-options=-L/usr/lib

test : src/Asteroids.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@

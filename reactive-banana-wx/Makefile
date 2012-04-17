.PHONY: all test

APPS=Asteroids Counter CRUD CurrencyConverter NetMonitor TicTacToe TwoCounters Wave
OBJ=dist/build
COMPILE= \
	ghc --make -i$(OBJ) -L/usr/lib -L$(OBJ) \
	-isrc -i../reactive-banana/src -idist/build/autogen \
	-DbuildExamples

# Compile all programs with cabal
all:
	cabal configure -fbuildExamples && cabal build --ghc-options=-L/usr/lib


# Compile individual programs for testing
Arithmetic : src/Arithmetic.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

Asteroids : src/Asteroids.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@ \
	    && cp data/* $@.app/Contents/Resources

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

CRUD : src/CRUD.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

CurrencyConverter : src/CurrencyConverter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

NetMonitor : src/NetMonitor.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

TicTacToe : src/TicTacToe.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

TwoCounters : src/TwoCounters.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@

Wave : src/Wave.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ && macosx-app $@


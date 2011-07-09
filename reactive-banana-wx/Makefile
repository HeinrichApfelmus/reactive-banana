.PHONY: all clean open

APPS=Asteroids Counter CurrencyConverter TwoCounters Wave
OBJ=dist/build
COMPILE=ghc --make -i$(OBJ) -L$(OBJ) -isrc -i../reactive-banana/src

all: $(APPS)

Asteroids : src/Asteroids.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@

CurrencyConverter : src/CurrencyConverter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@

TwoCounters : src/TwoCounters.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@
		
Wave : src/Wave.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/
	macosx-app $@

clean:
	rm -rf $(APPS) $(OBJ)/*.o $(OBJ)/*.hi *.app *.exe *.manifest

open: Guitar
	open Guitar.app
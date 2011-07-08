.PHONY: all clean open

APPS=Counter
OBJ=dist/build
COMPILE=ghc --make -i$(OBJ) -L$(OBJ) -isrc -i../reactive-banana/src

all: TwoCounters Counter Asteroids

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o Counter src/Counter.hs -outputdir $(OBJ)/Counter.tmp/
	macosx-app Counter

TwoCounters : src/TwoCounters.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o TwoCounters src/TwoCounters.hs -outputdir $(OBJ)/TwoCounters.tmp/
	macosx-app TwoCounters

Asteroids : src/Asteroids.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o Asteroids src/Asteroids.hs -outputdir $(OBJ)/Asteroids.tmp/
	macosx-app Asteroids

clean:
	rm -rf $(APPS) $(OBJ)/*.o $(OBJ)/*.hi *.app *.exe *.manifest

open: all
	open TwoCounters.app
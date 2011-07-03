.PHONY: all clean open

APPS=Counter
OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc -i../reactive-banana/src

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $<
	macosx-app $@	

TwoCounters : src/TwoCounters.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $<
	macosx-app $@

all: TwoCounters

clean:
	rm -rf $(APPS) $(OBJ)/*.o $(OBJ)/*.hi *.app *.exe *.manifest

open: all
	open TwoCounters.app
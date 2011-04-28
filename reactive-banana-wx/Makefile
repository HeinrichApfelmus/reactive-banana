.PHONY: all clean

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc -i../reactive-banana/src

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(COMPILE) -o $@ $<
	macosx-app $@	


all: Counter

clean:
	rm -rf $(APPS) obj/*.o obj/*.hi *.app *.exe *.manifest

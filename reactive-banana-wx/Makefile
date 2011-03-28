.PHONY: all clean

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

Counter : src/Counter.hs src/Reactive/WX.hs
	$(COMPILE) -o $@ $<
	macosx-app $@	


all: Counter

clean:
	rm -rf $(APPS) obj/*.o obj/*.hi *.app *.exe *.manifest

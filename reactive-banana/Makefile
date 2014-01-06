.PHONY: all SpaceLeaks profile

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

all : SpaceLeaks

SpaceLeaks : src/Reactive/Banana/Prim/Test.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ -prof \
	  -main-is Reactive.Banana.Prim.Test.main

profile : SpaceLeaks
	./SpaceLeaks +RTS -hc -L50 && hp2ps -c SpaceLeaks.hp
	./SpaceLeaks +RTS -prof

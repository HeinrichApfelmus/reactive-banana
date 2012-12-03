.PHONY: all SpaceLeaks

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

all : SpaceLeaks

SpaceLeaks : src/Reactive/Banana/Test/SpaceLeaks.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ -prof
	# -auto-all

profile : SpaceLeaks
	./SpaceLeaks +RTS -hc -L50 && hp2ps -c SpaceLeaks.hp
	./SpaceLeaks +RTS -prof

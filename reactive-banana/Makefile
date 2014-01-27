.PHONY: all SpaceLeaks space profile

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

all : SpaceLeaks

SpaceLeaks : src/Reactive/Banana/Prim/Test.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ -prof \
	  -main-is Reactive.Banana.Prim.Test.main

space : SpaceLeaks
	./SpaceLeaks +RTS -hc -L50 && hp2ps -c SpaceLeaks.hp
	./SpaceLeaks +RTS -prof


Performance=src/Reactive/Banana/Test/Performance.hs
profile :
	$(COMPILE) -o Performance $(Performance) -outputdir $(OBJ)/Performance.tmp/ \
		-prof -O \
		-main-is Reactive.Banana.Test.Performance.main
	# ./Performance +RTS -hc -L50 2>/dev/null && hp2ps -c Performance.hp
	./Performance +RTS -prof 2>/dev/null

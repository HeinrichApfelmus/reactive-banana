.PHONY: all SpaceLeaks space profile core

OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

all : SpaceLeaks

SpaceLeaks : src/Reactive/Banana/Prim/Test.hs
	$(COMPILE) -o $@ $< -outputdir $(OBJ)/$@.tmp/ -prof \
	  -main-is Reactive.Banana.Prim.Test.main

space : SpaceLeaks
	./SpaceLeaks +RTS -hc -L50 && hp2ps -c SpaceLeaks.hp
	./SpaceLeaks +RTS -prof

# Compile and inspect core for the inner loop
MODULE=src/Reactive/Banana/Prim/Evaluation
core :
	$(COMPILE) -fforce-recomp -O -ddump-simpl \
		$(MODULE).hs > $(MODULE).core.hs

# Run performance test
Performance=src/Reactive/Banana/Test/Performance.hs
profile :
	$(COMPILE) -o Performance $(Performance) -outputdir $(OBJ)/Performance.tmp/ \
		-prof -O -auto-all \
		-main-is Reactive.Banana.Test.Performance.main
	# ./Performance +RTS -hc -L50 2>/dev/null && hp2ps -c Performance.hp
	./Performance +RTS -s
	./Performance +RTS -prof 2>/dev/null && mv Performance.prof Performance.prof.txt

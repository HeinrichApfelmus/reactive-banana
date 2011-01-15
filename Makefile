OBJ=obj
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

GUITest : src/GUITest.hs src/GUI.hs
	$(COMPILE) -o $@ $<
	macosx-app $@

BlackBoard : src/*.hs src/Reactive/*.hs src/Graphics/*.hs
	$(COMPILE) -o $@ $<
	macosx-app $@

.PHONY: clean, run

all: BlackBoard GUITest

clean:
	rm -rf $(APPS) obj/*.o obj/*.hi *.app *.exe *.manifest

run: BlackBoard
	open Blackboard.app

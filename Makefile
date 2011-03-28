.PHONY: clean run reactive-banana reactive-banana-wx
.PHONY: GUITest BlackBoard


OBJ=dist/build
COMPILE=ghc --make -outputdir $(OBJ) -i$(OBJ) -L$(OBJ) -isrc

#GUITest : src/GUITest.hs src/GUI.hs
#	$(COMPILE) -o $@ $<
#	macosx-app $@

BlackBoard : reactive-banana reactive-banana-wx BlackBoard/src/*.hs
	cd BlackBoard && $(COMPILE) -o BlackBoard src/BlackBoard.hs \
		&& macosx-app BlackBoard

all: BlackBoard reactive-banana reactive-banana-wx

clean:
	rm -rf $(APPS) obj/*.o obj/*.hi *.app *.exe *.manifest

run: BlackBoard
	open BlackBoard/BlackBoard.app

reactive-banana:
	cd reactive-banana && cabal build && cabal register --inplace
reactive-banana-wx:
	cd reactive-banana-wx && cabal build && cabal register --inplace

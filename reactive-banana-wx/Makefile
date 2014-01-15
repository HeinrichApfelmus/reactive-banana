.PHONY: all test

APPS=Asteroids Counter CRUD CurrencyConverter NetMonitor TicTacToe TwoCounters Wave
BUILD = cabal build --ghc-options=-L/usr/lib

# Compile all programs with cabal
all:
	cabal configure -fbuildExamples && $(BUILD)


# Compile individual programs for testing
Animation : src/Animation.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

Arithmetic : src/Arithmetic.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

Asteroids : src/Asteroids.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

BarTab : src/BarTab.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

Counter : src/Counter.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

CRUD : src/CRUD.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

CurrencyConverter : src/CurrencyConverter.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

NetMonitor : src/NetMonitor.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

TicTacToe : src/TicTacToe.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

TwoCounters : src/TwoCounters.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@

Wave : src/Wave.hs src/Reactive/Banana/WX.hs
	$(BUILD) $@


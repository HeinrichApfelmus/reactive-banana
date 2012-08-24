-- Build .app bundles with the cabal-macosx package
-- Modeled after the excellent documentation on 
-- https://github.com/gimbo/cabal-macosx/tree/master/examples

import Distribution.MacOSX
import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps =
    [mkApp filesAsteroids "Asteroids", mkApp filesAnimation "Animation"] ++ apps

apps = map (mkApp []) $
    words "Arithmetic BarTab Counter CurrencyConverter CRUD"
    ++ words "NetMonitor TicTacToe TwoCounters Wave"
filesAsteroids = map ("data/" ++) $
    words "burning.ico rock.ico ship.ico explode.wav"
filesAnimation = map ("data/" ++) $
    words "banana.png"


mkApp files name = MacApp name Nothing Nothing files [] DoNotChase

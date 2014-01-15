-- Build .app bundles with the cabal-macosx package
-- Modeled after the excellent documentation on 
-- https://github.com/gimbo/cabal-macosx/tree/master/examples

import Distribution.MacOSX as Mac
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
    { postBuild = myPostBuild -- no-op if not MacOS X
    }

myPostBuild a b c d = Mac.appBundleBuildHook (filterApps a guiApps) a b c d

filterApps :: [String] -> [MacApp] -> [MacApp]
filterApps args apps = if null names then apps else names
    where
    names = [ app
        | app@(MacApp name1 _ _ _ _ _) <- apps
        , name2 <- args
        , name1 == name2
        ]

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

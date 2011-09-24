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
guiApps = [MacApp "Asteroids"
                  Nothing
                  Nothing -- Build a default Info.plist for the icon.
                  files   -- bitmaps and .wav
                  [] -- No other binaries.
                  DoNotChase -- Try changing to ChaseWithDefaults
          ] ++ apps

apps = map app $
    words "Arithmetic Counter CurrencyConverter CRUD"
    ++ words "NetMonitor TicTacToe TwoCounters Wave"
app name = MacApp name Nothing Nothing [] [] DoNotChase

files = map ("data/" ++) $
    words "burning.ico rock.ico ship.ico explode.wav"
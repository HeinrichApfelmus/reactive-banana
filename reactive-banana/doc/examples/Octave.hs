{-----------------------------------------------------------------------------
    reactive-banana
    
    Example: "The world's worst synthesizer"
    from the unofficial tutorial.
    <http://www.haskell.org/haskellwiki/FRP_explanation_using_reactive-banana>
------------------------------------------------------------------------------}
module Main where

import Data.Char (toUpper)
import Control.Monad (forever)
import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)
import Reactive.Banana
import Reactive.Banana.Prim (addHandler)
import Reactive.Banana.Frameworks


type Octave = Int

data Pitch = PA | PB | PC | PD | PE | PF | PG
    deriving (Eq, Enum)

-- Mapping between pitch and the char responsible for it.
pitchChars :: [(Pitch, Char)]
pitchChars = [(p, toEnum $ fromEnum 'a' + fromEnum p) |
              p <- [PA .. PG]]

-- Reverse of pitchChars
charPitches :: [(Char, Pitch)]
charPitches = [(b, a) | (a, b) <- pitchChars]

data Note = Note Octave Pitch

instance Show Pitch where
    show p = case lookup p pitchChars of
        Nothing -> error "cannot happen"
        Just c  -> [toUpper c]

instance Show Note where
    show (Note o p) = show p ++ show o

-- Filter and transform events at the same time.
filterMapJust :: (a -> Maybe b) -> Event t a -> Event t b
filterMapJust f = filterJust . fmap f

-- Change the original octave by adding a number of octaves, taking
-- care to limit the resulting octave to the 0..10 range.
changeOctave :: Int -> Octave -> Octave
changeOctave d = max 0 . min 10 . (d+)

-- Get the octave change for the '+' and '-' chars.
getOctaveChange :: Char -> Maybe Int
getOctaveChange c = case c of
    '+' -> Just 1
    '-' -> Just (-1)
    _ -> Nothing

makeNetworkDescription :: Frameworks t => AddHandler Char -> Moment t ()
makeNetworkDescription addKeyEvent = do
    eKey <- fromAddHandler addKeyEvent
    let
        eOctaveChange = filterMapJust getOctaveChange eKey
        bOctave = accumB 3 (changeOctave <$> eOctaveChange)
        ePitch = filterMapJust (`lookup` charPitches) eKey
        bPitch = stepper PC ePitch
        bNote = Note <$> bOctave <*> bPitch
        foo = Note 0 PA
    eNoteChanged <- changes bNote
    reactimate' $ fmap (\n -> putStrLn ("Now playing " ++ show n))
                 <$> eNoteChanged

main :: IO ()
main = do
    (addKeyEvent, fireKey) <- newAddHandler
    network <- compile (makeNetworkDescription addKeyEvent)
    actuate network
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    forever (getChar >>= fireKey)

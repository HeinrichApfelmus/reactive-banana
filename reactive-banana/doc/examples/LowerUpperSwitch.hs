{-----------------------------------------------------------------------------
    reactive-banana

    Example: Switch between lowercase and uppercase transformations on strings
------------------------------------------------------------------------------}
module Main where

import Data.List (isPrefixOf)
import Data.Char (toUpper, toLower)
import Control.Event.Handler
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = do
    (addHandler, fire) <- newAddHandler
    compile (network addHandler) >>= actuate
    fire "should be lower, because the initial activeTransformationBehavior is toLower"
    fire "UPPER; note, changes to behaviors are not visible until the next fire, so this line will be lower"
    fire "this line should be upper"
    fire "lower; note, changes to behaviors are not visible until the next fire, so this line will still be upper"
    fire "this should be lower again"

network :: AddHandler String -> MomentIO ()
network addHandler = do
    fireEvent <- fromAddHandler addHandler
    let toLowerBehavior :: Behavior (String -> String)
        toLowerBehavior = pure (map toLower)
        toUpperBehavior :: Behavior (String -> String)
        toUpperBehavior = pure (map toUpper)
        lowerEvent :: Event (Behavior (String -> String))
        lowerEvent = const toLowerBehavior <$> filterE ("lower" `isPrefixOf`) fireEvent
        upperEvent :: Event (Behavior (String -> String))
        upperEvent = const toUpperBehavior <$> filterE ("UPPER" `isPrefixOf`) fireEvent
    activeTransformationBehavior <- switchB toLowerBehavior (unionWith const lowerEvent upperEvent)
    reactimate $ putStrLn <$> (activeTransformationBehavior <@> fireEvent)

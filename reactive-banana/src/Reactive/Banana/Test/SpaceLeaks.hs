{-----------------------------------------------------------------------------
    reactive-banana

    Testing for particular space use
------------------------------------------------------------------------------}
import Control.Exception (evaluate)
import Control.Monad (forM_)

import Reactive.Banana
import Reactive.Banana.Frameworks

main = test_stepper_strict

{-----------------------------------------------------------------------------
    Space Leaks
    The following example should not leak space
------------------------------------------------------------------------------}
-- Should throw an exception because  accumE  is strict in the event values.
test_accumE_strict :: IO ()
test_accumE_strict = do
    (handler, fire) <- newAddHandler
    network <- compile $ do
        event1 <- fromAddHandler handler
        let event2 = accumE () $ undefined <$ event1
        reactimate $ return () <$ event2
    actuate network

    forM_ [1..80000] $ \n -> fire ()

-- should not leak space
test_stepper_strict :: IO ()
test_stepper_strict = do
    (handler, fire) <- newAddHandler
    network <- compile $ do
        event <- fromAddHandler handler
        -- let behavior = accumB () (fmap undefined event)
        let behavior = accumB () (id <$ event)
        -- let behavior = stepper () event
        let event2   = (\b e -> e) <$> behavior <@> event
        reactimate $ evaluate <$> event2
    actuate network

    forM_ [1..8000] $ \n -> fire ()




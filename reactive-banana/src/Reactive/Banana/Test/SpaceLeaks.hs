{-----------------------------------------------------------------------------
    reactive-banana

    Testing for particular space use
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception (evaluate)
import Control.Monad (forM_)

import Reactive.Banana
import Reactive.Banana.Frameworks

main = test_behavior_apply

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

-- reported by  bjoeris
test_behavior_apply :: IO ()
test_behavior_apply = do
    (handler, fire) <- newAddHandler
    let
        networkDescription :: forall t. (Frameworks t) => Moment t ()
        networkDescription = do
            foo <- fromAddHandler handler
            -- let bar = (pure (const True) :: Behavior t (() -> Bool)) `apply` foo
            
            -- this line has the same problem:
            let bar = (pure (const True) :: Behavior t (()->Bool)) `apply` never :: Event t Bool 

            -- this line doesn't
            -- let bar = (const True) `fmap` foo                              
            
            reactimate $ fmap (const $ return ()) foo
            
            -- commenting out this line fixes the problem
            reactimate $ fmap (const $ return ()) bar

    network <- compile networkDescription
    actuate network
    forM_ [1..3*10^4] $ \n -> fire ()

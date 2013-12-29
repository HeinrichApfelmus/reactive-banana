{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Banana.Prim.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import           Data.Unique.Really
import qualified Data.Vault.Strict  as Vault

import Reactive.Banana.Prim.Monads
import Reactive.Banana.Prim.Types

import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Create Pulse and Latch
------------------------------------------------------------------------------}
{-
* Note [LatchCreation]

When creating a new latch from a pulse, we assume that the
pulse cannot fire at the moment that the latch is created.
This is important when switching latches, because of note [PulseCreation].

When creating a latch, we have to write its past value
into the network state. In particular, when we are in an  EvalP  context,
this value has to present.

* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for desaster.

* Note [unsafePerformIO]

We're using @unsafePerformIO@ only to get @Key@ and @Unique@.
It's not great, but it works.

Unfortunately, using @IO@ as the base of the @Network@ monad
transformer doens't work because it doesn't support recursion
and @mfix@ very well.

We could use the @ST@ monad, but this would add a type parameter
to everything. A refactoring of this scope is too annoying for
my taste right now.

-}

-- make pulse from evaluation function
mkPulse :: EvalP (Maybe a) -> Build (Pulse a)
mkPulse eval = unsafePerformIO $ do
    key <- Vault.newKey
    uid <- newUnique
    return $ do
        let write = maybe (return ()) (writePulseP key)
        return $ Pulse
            { evaluateP = {-# SCC evaluateP #-} write =<< eval
            , getValueP = Vault.lookup key
            , uidP      = uid
            }

neverP :: Build (Pulse a)
neverP = debug "neverP" $ unsafePerformIO $ do
    uid <- newUnique
    return $ return $ Pulse
        { evaluateP = return ()
        , getValueP = const Nothing
        , uidP      = uid
        }


{-

* Note [LatchStrictness]

Any value that is stored in the graph over a longer
period of time must be stored in WHNF.

This implies that the values in a latch must be forced to WHNF
when storing them. That doesn't have to be immediately
since we are tying a knot, but it definitely has to be done
before  evaluateGraph  is done.

It also implies that reading a value from a latch must
be forced to WHNF before storing it again, so that we don't
carry around the old collection of latch values.
This is particularly relevant for `applyL`.

Conversely, since latches are the only way to store values over time,
this is enough to guarantee that there are no space leaks in this regard.

-}

-- make latch from initial value and an evaluation function
mkLatch :: a -> EvalL (Maybe a) -> Build (Latch a)
mkLatch now eval = unsafePerformIO $ do
    key <- Vault.newKey
    uid <- {-# SCC "latch/newUnique" #-} newUnique
    return $ do
        -- Initialize with current value.
        -- See note [LatchCreation].
        addLatchValue key now
        
        let write = maybe (return ()) (writeLatchL key)
        let err = error "getValueL: latch not initialized"
        
        return $ Latch
            { evaluateL = {-# SCC evaluateL #-} write =<< eval
            , getValueL = {-# SCC getValueL #-} maybe err id . Vault.lookup key
            , uidL      = uid
            }

pureL :: a -> Build (Latch a)
pureL a = debug "pureL" $ unsafePerformIO $ do
    uid <- liftIO newUnique
    return $ return $ Latch
        { evaluateL = return ()
        , getValueL = const a
        , uidL      = uid
        }

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
stepperL :: a -> Pulse a -> Build (Latch a)
stepperL a p = debug "stepperL" $ do
    x <- mkLatch a $ {-# SCC stepperL #-} readPulseL p
    L x `dependOn` P p
    return x

accumP :: a -> Pulse (a -> a) -> Build (Pulse a)
accumP a p = debug "accumP" $ mdo
    x       <- stepperL a result
    result  <- mkPulse $
        {-# SCC accumP #-} (\x -> fmap ($ x)) <$> readLatchP x <*> readPulseP p
    -- Evaluation order of the result pulse does *not*
    -- depend on the latch. It does depend on latch value,
    -- though, so don't garbage collect that one.
    P result `dependOn` P p
    return result

applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = debug "applyP" $ do
    result <- mkPulse $ {-# SCC applyP #-} fmap <$> readLatchP f <*> readPulseP x
    P result `dependOn` P x
    return result

-- Tag a pulse with future values of a latch.
-- Caveat emptor. These values are not defined until after the  EvalL  phase.
tagFuture :: Latch a -> Pulse b -> Build (Pulse a)
tagFuture f x = debug "tagFuture" $ do
    result <- mkPulse $ fmap . const <$> readLatchFutureP f <*> readPulseP x
    P result `dependOn` P x
    return result


mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p = debug "mapP" $ do
    result <- mkPulse $ {-# SCC mapP #-} fmap f <$> readPulseP p
    P result `dependOn` P p
    return result

filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p = debug "filterJustP" $ do
    result <- mkPulse $ {-# SCC filterJustP #-} join <$> readPulseP p
    P result `dependOn` P p
    return result

unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f px py = debug "unionWith" $ do
        result <- mkPulse $
            {-# SCC unionWithP #-} eval <$> readPulseP px <*> readPulseP py
        P result `dependOns` [P px, P py]
        return result
    where
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing  = Just x
    eval Nothing  (Just y) = Just y
    eval Nothing  Nothing  = Nothing


applyL :: Latch (a -> b) -> Latch a -> Build (Latch b)
applyL lf lx = debug "applyL" $ do
    -- see note [LatchStrictness]
    let evalL = {-# SCC applyL #-} ($!) <$> readLatchL lf <*> readLatchL lx
    now    <- ($) <$> readLatchB lf <*> readLatchB lx
    result <- mkLatch now $ Just <$> evalL
    L result `dependOns` [L lf, L lx]
    return result

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
executeP :: Pulse (BuildIO a) -> Build (Pulse a)
executeP pn = do
    result <- mkPulse $ do
        mp <- readPulseP pn
        case mp of
            Just p  -> liftBuildIOP $ Just <$> p
            Nothing -> return Nothing
    P result `dependOn` P pn
    return result

switchP :: Pulse (Pulse a) -> Build (Pulse a)
switchP pp = mdo
    never <- neverP
    lp    <- stepperL never pp
    let
        eval = do
            newPulse <- readPulseP pp
            case newPulse of
                Nothing -> return ()
                Just p  -> liftBuildP $
                                  P result `dependOn` P p  -- check in new pulse
            readPulseP =<< readLatchP lp                   -- fetch value from old pulse
    result <- mkPulse eval
    P result `dependOns` [L lp, P pp]
    return result


switchL :: Latch a -> Pulse (Latch a) -> Build (Latch a)
switchL l p = mdo
    ll <- stepperL l p2
    let -- register the new dependency
        evalP = do
            ml <- readPulseP p
            case ml of
                Just l -> do
                    liftBuildP $ L result `dependOn` L l
                    return $ Just l
                Nothing -> return Nothing
    p2 <- mkPulse $ evalP
    
    let -- calculate value of result latch
        evalL = readLatchL =<< readLatchL ll
    
    now    <- readLatchB l
    result <- mkLatch now $ Just <$> evalL
    L result `dependOns` [L l, L ll, P p]
    return result



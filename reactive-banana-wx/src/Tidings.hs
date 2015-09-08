{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
module Tidings (
    -- * Synopsis
    -- The 'Tidings' data type for composing user events.
    --
    -- See <http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html>
    -- for more information.
    
    -- * Documentation
    Tidings, tidings, facts, rumors,
    ) where

import Reactive.Banana.Combinators

-- | Data type representing a behavior 'facts'
-- and suggestions to change it 'rumors'.
data Tidings a = T { facts :: Behavior a, rumors :: Event a }

-- | Smart constructor. Combine facts and rumors into 'Tidings'.
tidings :: Behavior a -> Event a -> Tidings a
tidings b e = T b e

instance Functor Tidings where
    fmap f (T b e) = T (fmap f b) (fmap f e)

-- | The applicative instance combines 'rumors'
-- and uses 'facts' when some of the 'rumors' are not available.
instance Applicative Tidings where
    pure x  = T (pure x) never
    f <*> x = uncurry ($) <$> pair f x

pair :: Tidings a -> Tidings b -> Tidings (a,b)
pair (T bx ex) (T by ey) = T b e
    where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\(x,_) (_,y) -> (x,y)) x y

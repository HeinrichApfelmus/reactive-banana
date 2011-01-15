import Graphic
import Reactive

    -- a gate, switching between two event streams
    -- True  == left stream
    -- False == right stream
gate :: Behavior Bool -> Event a -> Event a -> Event a
gate bb ex ey = onlyChanges $ (go `fmap` bb) `apply` (ex `mergeE` ey)
    where
    go True  (Left  x) = Change x
    go False (Right y) = Change y
    go _ _ = Keep


    -- guard depending on a behavior
guardBehavior :: Behavior Bool -> Event a -> Event a
guardBehavior bf = filterBehavior (fmap const bf)

    -- filter on a time-varying predicate
filterBehavior :: Behavior (a -> Bool) -> Event a -> Event a
filterBehavior bf ex = filterChanges $ (go `fmap` bf) `apply` ex
    where
    go f x = if f x then Change x else Keep

Behaviors
=========
Make it possible to obtain Behaviors from polling. (Oops, that should have been included right from the start.)

    fromPoll :: IO a -> NetworkDescription (Behavior a)


Convenience
===========
Convenience function for writing custom GUI widgets in FRP style.

    interpretAsHandler :: (Event a -> Event b) -> (AddHandler a -> AddHandler b)
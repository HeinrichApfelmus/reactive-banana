{-----------------------------------------------------------------------------
    reactive-banana-wx
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"

import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    f         <- frame [text := "Beispiel 2"]
    input1    <- entry f []
    output    <- staticText f [color := red]
    button1   <- button f [text := "Show"]
    
    set f [layout := margin 10 $ column 10 $
            [widget input1 
            ,widget button1
            ,minsize (sz 40 20) $ widget output]
          ]

    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
        
        binput1 <- behaviorText input1 ""
        -- binput1 :: Behavior t String
        ebutton1 <- event0 button1 command
        -- ebutton1 :: Event t ()

        let 
            einput1 :: Event t String
            einput1 = binput1 <@ ebutton1

            bresult :: Behavior t String
            bresult = reverse <$> stepper "" einput1

        sink output [text :== bresult]   

    network <- compile networkDescription    
    actuate network



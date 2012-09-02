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
    f         <- frame [text := "Beispiel 3"]
    output    <- staticText f [color := red]
    button1   <- button f [text := "1"]
    button2   <- button f [text := "2"]
    
    set f [layout := margin 10 $ column 10 $
            [widget button1 
            ,widget button2
            ,minsize (sz 40 20) $ widget output]
          ]

    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = do
        
        -- let ebutton1 = event0 button1 command

        ebutton1 <- event0 button1 command
        ebutton2 <- event0 button2 command

        let 
            ebutton :: Event t Bool
            ebutton = unionWith const
                        (True  <$ ebutton1)
                        (False <$ ebutton2)

            bbutton :: Behavior t Bool
            bbutton = stepper True ebutton

            eresult :: Event t String
            eresult = foo <$> bbutton <@> ebutton

            foo :: Bool -> Bool -> String
            foo old new
                | old == new = "Banane"
                | otherwise  = "Erdbeere"

            bresult :: Behavior t String
            bresult = stepper "" eresult

        sink output [text :== bresult]   

    network <- compile networkDescription    
    actuate network



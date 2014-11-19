import          Graphics.UI.Gtk
import          Data.Vector
import          Reactive.Banana
import          Reactive.Banana.Frameworks
import          Reactive.Banana.GTK
import          Text.Read

validateInput :: Maybe Integer -> String
validateInput Nothing = "Invalid input"
validateInput (Just int) = show int

buildRow :: IO (HBox, Entry, Label)
buildRow = do
    entry <- entryNew
    label <- labelNew (Just "Enter text")

    -- Layout
    hbox <- hBoxNew True 0
    containerAdd hbox entry
    containerAdd hbox label
    return (hbox, entry, label)

buttonEvent :: (Frameworks t, ButtonClass b) => b ->  Moment t (Event t ())
buttonEvent = flip event0 $ buttonActivated


networkAddingInput :: (ContainerClass w, ButtonClass b, Frameworks t) => w -> b ->  Moment t ()
networkAddingInput widget button = do
    eButton <- buttonEvent button 
    let newNetwork :: Frameworks t1 => Moment t1 ()
        newNetwork = do
            (w, entry, label) <- liftIO buildRow
            liftIO $ containerAdd widget w
            liftIO $ widgetShowAll w
            networkForInput entry label

    execute $ (FrameworksMoment newNetwork <$ eButton)
    return ()

networkForInput :: (EntryClass e, EditableClass e, LabelClass l, Frameworks t) => e -> l -> Moment t ()
networkForInput entry label = do
    eEntry <- eventEntry entry
    let bEntry = stepper "" eEntry
        bMaybe = readMaybe <$> bEntry
    sink label [labelLabel :== validateInput <$> bMaybe]

main :: IO ()
main = do
    initGUI
    window <- windowNew
    button <- buttonNew
    (initBox, entry, label) <- buildRow

    -- Layout
    vbox <- vBoxNew True 0
    containerAdd vbox button
    containerAdd vbox initBox

    -- Setting attribute
    set button [buttonLabel := "Add More Input"]
    set window [ containerBorderWidth := 10, containerChild := vbox]
    
    -- Network
    network <- compile $ networkForInput entry label >> networkAddingInput vbox button
    actuate network
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI

{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example:
    Small database with CRUD operations and filtering.
    To keep things simple, the list box is rebuild every time
    that the database is updated. This is perfectly fine for rapid prototyping.
    A more sophisticated approach would use incremental updates.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"
{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}

import Prelude hiding (lookup)
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Graphics.UI.WX as WX
import Graphics.UI.WX hiding (Event)
import qualified Graphics.UI.WXCore as WXCore
import Reactive.Banana
import Reactive.Banana.WX

import Tidings

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = start $ do
    -- GUI layout
    f           <- frame    [ text := "CRUD Example (Simple)" ]
    listBox     <- singleListBox f []
    createBtn   <- button f [ text := "Create" ]
    deleteBtn   <- button f [ text := "Delete" ]
    filterEntry <- entry  f [ ]
    
    firstname <- entry f [ ]
    lastname  <- entry f [ ]
    
    let dataItem = grid 10 10 [[label "First Name:", widget firstname]
                              ,[label "Last Name:" , widget lastname]]
    set f [layout := margin 10 $
            grid 10 5
                [[row 5 [label "Filter prefix:", widget filterEntry], glue]
                ,[minsize (sz 200 300) $ widget listBox, dataItem]
                ,[row 10 [widget createBtn, widget deleteBtn], glue]
                ]]

    -- event network
    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = mdo
            -- events from buttons
            eCreate <- event0 createBtn command       
            eDelete <- event0 deleteBtn command
            -- filter string
            tFilterString <- reactiveTextEntry filterEntry bFilterString
            let bFilterString = stepper "" $ rumors tFilterString
                tFilter = isPrefixOf <$> tFilterString
                bFilter = facts  tFilter
                eFilter = rumors tFilter

            -- list box with selection
            eSelection <- rumors <$> reactiveListDisplay listBox
                bListBoxItems bSelection bShowDataItem
            -- data item display
            eDataItemIn <- rumors <$> reactiveDataItem (firstname,lastname)
                bSelectionDataItem

            let -- database
                bDatabase :: Behavior t (Database DataItem)
                bDatabase = accumB emptydb $ unions
                    [ create ("Emil","Example") <$ eCreate
                    , filterJust $ update' <$> bSelection <@> eDataItemIn
                    , delete <$> filterJust (bSelection <@ eDelete)
                    ]
                    where
                    update' mkey x = flip update x <$> mkey
                
                -- selection
                bSelection :: Behavior t (Maybe DatabaseKey)
                bSelection = stepper Nothing $ unions
                    [ eSelection
                    , Nothing <$ eDelete
                    , Just . nextKey <$> bDatabase <@ eCreate
                    , (\b s p -> b >>= \a -> if p (s a) then Just a else Nothing)
                        <$> bSelection <*> bShowDataItem <@> eFilter
                    ]
                
                bLookup :: Behavior t (DatabaseKey -> Maybe DataItem)
                bLookup = flip lookup <$> bDatabase
                
                bShowDataItem :: Behavior t (DatabaseKey -> String)
                bShowDataItem = (maybe "" showDataItem .) <$> bLookup
                
                bListBoxItems :: Behavior t [DatabaseKey]
                bListBoxItems = (\p show -> filter (p. show) . keys)
                    <$> bFilter <*> bShowDataItem <*> bDatabase

                bSelectionDataItem :: Behavior t (Maybe DataItem)
                bSelectionDataItem = (=<<) <$> bLookup <*> bSelection

            -- automatically enable / disable editing
            let
                bDisplayItem :: Behavior t Bool
                bDisplayItem = maybe False (const True) <$> bSelection
            
            sink deleteBtn [ enabled :== bDisplayItem ]
            sink firstname [ enabled :== bDisplayItem ]
            sink lastname  [ enabled :== bDisplayItem ]
    
    network <- compile networkDescription    
    actuate network

{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptydb = Database 0 Map.empty
keys    = Map.keys . db

create x     (Database newkey db) = Database (newkey+1) $ Map.insert newkey x db
update key x (Database newkey db) = Database newkey     $ Map.insert key    x db
delete key   (Database newkey db) = Database newkey     $ Map.delete key db
lookup key   (Database _      db) = Map.lookup key db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
type DataItem = (String, String)
showDataItem (firstname, lastname) = lastname ++ ", " ++ firstname

-- single text entry
reactiveTextEntry :: Frameworks t
    => TextCtrl a
    -> Behavior t String              -- text value
    -> Moment t (Tidings t String)    -- user changes
reactiveTextEntry w btext = do
    eUser <- eventText w        -- user changes

    -- filter text setting that are simultaneous with user events
    itext <- initial btext
    etext <- changes btext
    let etext2 = fst $ split $ unionWith (curry snd) (Left <$> etext) (Right <$> eUser)
        btext2 = stepper itext etext2

    sink w [ text :== btext2 ]  -- display value
    return $ tidings btext eUser

-- whole data item (consisting of two text entries)
reactiveDataItem :: Frameworks t
    => (TextCtrl a, TextCtrl b)
    -> Behavior t (Maybe DataItem)
    -> Moment t (Tidings t DataItem)
reactiveDataItem (firstname,lastname) binput = do
    t1 <- reactiveTextEntry firstname (fst . maybe ("","") id <$> binput)
    t2 <- reactiveTextEntry lastname  (snd . maybe ("","") id <$> binput)
    return $ (,) <$> t1 <*> t2


{-----------------------------------------------------------------------------
    reactive list display
    
    Display a list of (distinct) items in a list box.
    The current selection contains one or no items.
    Changing the set may unselect the current item,
        but will not change it to another item.
------------------------------------------------------------------------------}
reactiveListDisplay :: forall t a b. (Ord a, Frameworks t)
    => SingleListBox b          -- ListBox widget to use
    -> Behavior t [a]           -- list of items
    -> Behavior t (Maybe a)     -- selected element
    -> Behavior t (a -> String) -- display an item
    -> Moment t
        (Tidings t (Maybe a))   -- current selection as item (possibly empty)
reactiveListDisplay w bitems bsel bdisplay = do
    -- animate output items
    liftIO $ putStrLn "test"
    sink w [ items :== map <$> bdisplay <*> bitems ]
   
    -- animate output selection
    let bindices :: Behavior t (Map.Map a Int)
        bindices = (Map.fromList . flip zip [0..]) <$> bitems
        bindex   = (\m a -> maybe (-1) id $ flip Map.lookup m =<< a) <$>
                    bindices <*> bsel
    sink w [ selection :== bindex ]

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 :: Behavior t (Map.Map Int a)
        bindices2 = Map.fromList . zip [0..] <$> bitems
    esel <- eventSelection w
    return $ tidings bsel $ flip Map.lookup <$> bindices2 <@> esel


{-----------------------------------------------------------------------------
    wxHaskell convenience wrappers and bug fixes
------------------------------------------------------------------------------}
{- Currently exported from Reactive.Banana.WX

-- user input event - text for text entries
eventText :: TextCtrl w -> Moment t (Event t String)
eventText w = do
    -- Should probably be  wxEVT_COMMAND_TEXT_UPDATED ,
    -- but that's missing from wxHaskell.
    -- Note: Observing  keyUp events does create a small lag
    addHandler <- liftIO $ event1ToAddHandler w keyboardUp
    fromAddHandler $ mapIO (const $ get w text) addHandler

-- observe "key up" events (many thanks to Abu Alam)
-- this should probably be in the wxHaskell library
keyboardUp  :: WX.Event (Window a) (EventKey -> IO ())
keyboardUp  = WX.newEvent "keyboardUp" WXCore.windowGetOnKeyUp WXCore.windowOnKeyUp

-- user input event - selection marker for list events
eventSelection :: SingleListBox b -> Moment t (Event t Int)
eventSelection w = do
    liftIO $ fixSelectionEvent w
    addHandler <- liftIO $ event1ToAddHandler w (event0ToEvent1 select)
    fromAddHandler $ mapIO (const $ get w selection) addHandler

-- Fix @select@ event not being fired when items are *un*selected.
fixSelectionEvent listbox =
    liftIO $ set listbox [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get listbox selection
        when (s == -1) $ (get listbox (on select)) >>= id
-}

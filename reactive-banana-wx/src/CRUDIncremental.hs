{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: ListBox with CRUD operations
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. NetworkDescription t"
{-# LANGUAGE RecursiveDo #-}

import Control.Monad (join)
import qualified Data.List
import Data.Maybe
import qualified Data.Map as Map

import Graphics.UI.WX as WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main = start $ do
    -- GUI layout
    f        <- frame    [ text := "CRUD Example" ]
    listBox  <- singleListBox f []
    create   <- button f [ text := "Create" ]
    delete   <- button f [ text := "Delete" ]
    filter   <- entry f  [ processEnter := True ]
    
    name     <- entry f  [ processEnter := True ]
    surname  <- entry f  [ processEnter := True ]
    
    let dataItem = grid 10 10 [[label "Name:", widget name]
                              ,[label "Surname:", widget surname]]
    set f [layout := margin 10 $
            grid 10 5
                [[row 5 [label "Filter prefix:", widget filter], glue]
                ,[minsize (sz 200 300) $ widget listBox, dataItem]
                ,[row 10 [widget create, widget delete], glue]
                ]]

    -- event network
    let networkDescription :: forall t. NetworkDescription t ()
        networkDescription = mdo
            -- events from buttons
            eDelete <- event0 delete command
            eCreate <- event0 create command       
        
            -- time-varying value corresponding to the filter string
            (bFilter, eFilter) <- reactimateTextEntry filter (pure "")
            let dFilter = stepperD "" $ bFilter <@ eFilter
        
            -- list box with selection
            dSelectedItem <- reactimateListBox listBox database dFilter
            -- data corresponding to the selected item in the list box
            (inDataItem, changeDataItem)
                <- reactimateDataItem (name, surname) outDataItem
        
            let
                -- update the database whenever
                -- a data item is created, updated or deleted
                database :: DatabaseTime DataItem
                database = accumDatabase $
                    (Create Nothing ("Emil","Example") <$  eCreate)
                    `union` (Update <$> dSelectedItem  <@>
                                (inDataItem <@ changeDataItem))
                    `union` (Delete <$> dSelectedItem  <@  eDelete )
            
                -- display the data item whenever the selection changes
                outDataItem = stepperD ("","") $
                    lookup <$> valueDB database <@> changes dSelectedItem
                    where
                    lookup database m = fromMaybe ("","") $
                        readDatabase database =<< m

            -- automatically enable / disable editing
            let dDisplayItem = isJust <$> dSelectedItem
            sink delete  [ enabled :== dDisplayItem ]
            sink name    [ enabled :== dDisplayItem ]
            sink surname [ enabled :== dDisplayItem ]
    
    network <- compile networkDescription    
    actuate network

{-----------------------------------------------------------------------------
    Database Model
------------------------------------------------------------------------------}
-- Create/Update/Delete data type for efficient updates
data CUD key a
    = Create { getKey :: key, getItem :: a }
    | Update { getKey :: key, getItem :: a }
    | Delete { getKey :: key }

instance Functor (CUD key) where
    fmap f (Delete x) = Delete x
    fmap f cud = cud { getItem = f $ getItem cud }

isDelete (Delete _) = True
isDelete _ = False

-- Database type
type DatabaseKey = Int
data Database a  = Database { nextKey :: !Int, db :: Map.Map DatabaseKey a }

emptyDatabase = Database 0 Map.empty

-- Time-varying database,
-- similar to the Discrete type
data DatabaseTime a = DatabaseTime
    { valueDB   :: Behavior (Database a)
    , initialDB :: Database a
    , changesDB :: Event (CUD DatabaseKey a)
    }

-- accumulate a database from CUD operations
accumDatabase :: Event (CUD (Maybe DatabaseKey) a) -> DatabaseTime a
accumDatabase e = DatabaseTime valueDB initialDB changesDB
    where
    (changesDB, valueDB) = mapAccum initialDB $ acc <$> filterE valid e
    initialDB = emptyDatabase
    
    valid (Create Nothing _) = True
    valid cud = isJust $ getKey cud
    
    -- accumulation function
    acc (Create Nothing x)    (Database newkey db)  
        = (Create newkey x, Database (newkey+1) $ Map.insert newkey x db)
    acc (Update (Just key) x) (Database newkey db)
        = (Update key x, Database newkey $ Map.insert key x db)
    acc (Delete (Just key))   (Database newkey db)
        = (Delete key  , Database newkey $ Map.delete key db)

-- read a value from the database
readDatabase :: Database a -> DatabaseKey -> Maybe a
readDatabase (Database _ db) = flip Map.lookup db

{-----------------------------------------------------------------------------
    Data items that are stored in the data base
------------------------------------------------------------------------------}
type DataItem = (String, String)

-- text entry widgets in terms of discrete time-varying values
reactimateTextEntry
    :: TextCtrl a
    -> Discrete String      -- set text programmatically (view)
    -> NetworkDescription
        (Behavior String    -- current text (both view & controller)
        ,Event ())          -- user changes (controller)
reactimateTextEntry entry input = do
    sink entry [ text :== input ]

    -- event: Enter key
    eEnter <- event0 entry command
    -- event: text entry loses focus
    eLeave <- (() <$) . filterE not <$> event1 entry focus
    b <- behavior entry text
    return (b, eEnter `union` eLeave)

-- whole data item (consisting of two text entries)
reactimateDataItem
    :: (TextCtrl a, TextCtrl b)
    -> Discrete DataItem
    -> NetworkDescription
        (Behavior DataItem, Event ())
reactimateDataItem (name,surname) input = do
    (d1,e1) <- reactimateTextEntry name    (fst <$> input)
    (d2,e2) <- reactimateTextEntry surname (snd <$> input)
    return ( (,) <$> d1 <*> d2 , e1 `union` e2 )

-- custom show function
showDataItem (name, surname) = surname ++ ", " ++ name 

{-----------------------------------------------------------------------------
    List Box View
------------------------------------------------------------------------------}
-- Display the data base in a list box (view).
-- Also keep track of the currently selected item (controller).
reactimateListBox
    :: SingleListBox b                 -- list box widget
    -> DatabaseTime DataItem           -- database
    -> Discrete String                 -- filter string
    -> NetworkDescription
        (Discrete (Maybe DatabaseKey)) -- current selection as database key

reactimateListBox listBox database filter = do
    -- The list box keeps track
    -- of which data items are displayed, at which positions
    let (eListBoxUpdates, bDisplayMap)
            = mapAccum Map.empty
            $ (cudUpdate . fmap showDataItem <$> changesDB database)
              `union` (filterUpdate <$> valueDB database <@> changes filter)
    
    -- "animate" changes to the list box
    reactimate eListBoxUpdates
    -- debug: reactimate $ fmap print $ bDisplayMap <@ eListBoxUpdates
        
    -- event: item selection, maps to database key
    fixSelectionEvent listBox
    bSelection <- behavior listBox selection
    eSelect    <- event0   listBox select
    let eDelete   = filterE isDelete $ changesDB database
    return $ stepperD Nothing $
        -- event: item deleted 
        (Nothing <$ eDelete) `union`
        -- event: filter string changed
        (Nothing <$ changes filter) `union`
        -- event: user changes selection
        (lookupPositon <$> bSelection <*> bDisplayMap <@ eSelect)

    where

    -- turn CUD into a function that updates
    --   ( the graphics of the list box
    --   , the map from database keys to list positions )
    cudUpdate
        :: CUD DatabaseKey String -> DisplayMap -> (IO (), DisplayMap)

    cudUpdate (Create key str) display
        = (itemAppend listBox str, appendKey key display)
    cudUpdate (Update key str) display
        = case lookupKey key display of
            Just position -> (set listBox [ item position := str ], display)
            Nothing       -> (return (), display)
    cudUpdate (Delete key) display
        = case lookupKey key display of
            Just position -> (itemDelete listBox position
                             ,deleteKey key position display)
            Nothing       -> (return (), display)
    
    -- rebuild listBox when filter string changes
    filterUpdate database s _ = (set listBox [ items := xs ], display)
        where
        dat = Map.filter (s `Data.List.isPrefixOf`)
            . Map.map showDataItem . db $ database
        xs  = Map.elems dat
        display = Map.fromList $ zip (Map.keys dat) [0..] 


-- Map between database keys and their position in the list box
type DisplayMap = Map.Map DatabaseKey Int

lookupKey = Map.lookup
lookupPositon pos = fmap fst . Data.List.find ((pos ==) . snd) . Map.toList
appendKey key display = Map.insert key (Map.size display) display
deleteKey key position display
    = Map.delete key
    -- recalculate positions of the other elements
    . Map.map (\pos -> if pos > position then pos - 1 else pos)
    $ display

{-----------------------------------------------------------------------------
    wxHaskell bug fixes
------------------------------------------------------------------------------}
-- Fix @select@ event not being fired when items are *un*selected
fixSelectionEvent listbox =
    liftIO $ set listbox [ on unclick := handler ]
    where
    handler _ = do
        propagateEvent
        s <- get listbox selection
        when (s == -1) $ join $ get listbox (on select)
        




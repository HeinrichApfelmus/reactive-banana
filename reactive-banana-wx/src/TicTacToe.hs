{-----------------------------------------------------------------------------
    reactive-banana-wx
    
    Example: A version of TicTacToe with eclectic interface elements
    Original Author: Gideon Sireling
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import Data.Array
import Data.List hiding (union)

import Graphics.UI.WX hiding (Event)
import Graphics.UI.WXCore hiding (Event)

import Reactive.Banana
import Reactive.Banana.WX

{-----------------------------------------------------------------------------
    User Interface
------------------------------------------------------------------------------}

main :: IO ()
main = start $ do
    -- create the main window
    window <- frame [text := "OX"]
    label  <- staticText window [text := "Move: X"]
        -- overwritten by FRP, here to ensure correct positioning
    btns   <- replicateM 3 $ button window [size := sz 40 40]
    radios <- replicateM 3 $ radioBox window Vertical ["", " "] []
    checks <- replicateM 3 $ checkBox window [text := "  "]
        -- reserve space for X/O in label
    
    set window [layout := column 5 [grid 1 1
                    [map widget btns, map widget radios, map widget checks]
                    , floatCenter $ widget label]]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            -- convert WxHaskell events to FRP events
            let event0s widgets event = forM widgets $ \x -> event0 x event
            events <- liftM concat $ sequence
                [event0s btns command, event0s radios select, event0s checks command]
        
            let
                moves :: Event t (Game -> Game)
                moves = foldl1 union $ zipWith (\e s -> move s <$ e) events
                        [(x,y) | y <- [1..3], x <- [1..3]]
            
                state :: Behavior t Game
                state = accumB newGame moves
            
                currentPlayer :: Behavior t String
                currentPlayer = (show . player) <$> state
            
                tokens :: [Behavior t String]
                tokens = map (\e -> stepper "" (currentPlayer <@ e)) events
        
            -- wire up the widget event handlers
            zipWithM_ (\b e -> sink b [text :== e, enabled :== null <$> e])
                      (map objectCast btns
                      ++ map objectCast radios
                      ++ map objectCast checks :: [Control ()])
                      tokens
        
            sink label [text :== ("Move: " ++) <$> currentPlayer]
            
            -- end game event handler
            eState <- changes state
            reactimate $ end window <$> filterJust (isGameEnd . board <$> eState)
    
    network <- compile networkDescription    
    actuate network


end :: Frame () -> Token -> IO ()
end window result = do
    infoDialog window "" $
        case result of
            X    -> "X won!"
            O    -> "O won!"
            None -> "Draw!"
    close window

{-----------------------------------------------------------------------------
    Game Logic
------------------------------------------------------------------------------}
data Token = None | X | O
    deriving Eq

-- |The coordinates of a square.
type Square = (Int,Int)

-- |A noughts and crosses board.
type Board = Array Square Token

-- |Returns an empty 'Board'.
newBoard :: Board
newBoard = listArray ((1,1),(3,3)) (repeat None)

-- |Puts a 'Token' in a 'Square'.
setSquare :: Board -> Square -> Token -> Board
setSquare board square token =
    if (board ! square) /= None
    then error $ "square " ++ show square ++ " is not empty"
    else board // [(square, token)]

-- |Determine if the 'Board' is in an end state.
--  Returns 'Just' 'Token' if the game has been won,
-- 'Just' 'None' for a draw, otherwise 'Nothing'.
isGameEnd :: Board -> Maybe Token
isGameEnd board
    | Just X `elem` maybeWins = Just X
    | Just O `elem` maybeWins = Just O
    | None `notElem` elems board = Just None
    | otherwise = Nothing

    where rows :: [[Square]]
          rows = let i = [1..3]
                 in [[(x,y) | y <- i] | x <- i] ++ -- rows
                    [[(y,x) | y <- i] | x <- i] ++ -- coloumns
                    [[(x,x) | x <- i], [(x,4-x) | x <- i]] -- diagonals

          rows2tokens :: [[Token]]
          rows2tokens = map (map (board !)) rows

          isWin :: [Token] -> Maybe Token
          isWin tokens
              | all (==X) tokens = Just X
              | all (==O) tokens = Just O
              | otherwise = Nothing

          maybeWins :: [Maybe Token]
          maybeWins = map isWin rows2tokens

-- |The state of a game, i.e. the player who's turn it is, and the current board.
data Game = Game { player :: Token, board :: Board }

newGame :: Game
newGame = Game X newBoard

-- |Puts the player's token on the specified square.
-- Returns 'Just' 'Token' if the game has been won,
-- 'Just' 'None' for a draw, otherwise 'Nothing'.
move :: Square -> Game -> Game
move square (Game player board) = Game player' board'
    where
    board'  = setSquare board square player
    player' = case player of {X -> O; O -> X}

{-----------------------------------------------------------------------------
    Show instances
------------------------------------------------------------------------------}
outersperse :: a -> [a] -> [a]
outersperse x ys = x : intersperse x ys ++ [x]

instance Show Token where
    show X = "X"
    show O = "O"
    show None = " "
    showList tokens = showString $ outersperse '|' $ concatMap show tokens

-- Board cannot be declared an instance of Show,
-- as this would overlap with the existing instance for Array.
showBoard :: Board -> String
showBoard board =
    let border = " +-+-+-+"
        i = [1..3]
        showRow x = show x ++ show [board ! (y,x) | y <- i]
    in intercalate "\n" $ "  1 2 3" : outersperse border (map showRow i)

instance Show Game where
    show (Game player board) = showBoard board ++ "\n\nTurn: " ++ show player
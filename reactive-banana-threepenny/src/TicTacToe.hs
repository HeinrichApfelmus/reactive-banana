{-----------------------------------------------------------------------------
    reactive-banana-threepenny
    
    Example: TicTacToe.
    Original Author: Gideon Sireling
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Control.Monad
import Data.Array
import Data.List hiding (union)
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event, filterJust)

import Reactive.Banana
import Reactive.Banana.Threepenny

import Paths

{-----------------------------------------------------------------------------
    User Interface
------------------------------------------------------------------------------}
main :: IO ()
main = do
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = ""
        } setup

-- | Load images corresponding to tokens.
loadToken2URL :: Window -> IO (Token -> String)
loadToken2URL window = do
    let loadPng name = loadFile window "image/png" =<< getDataFile (name ++ ".png")
    [none, x, o] <- mapM loadPng ["empty","cross","circle"]
    return $ \token -> case token of { None -> none; X -> x; O -> o; }

-- | Set up user interface.
setup :: Window -> IO ()
setup window = do
    return window # set title "Tic Tac Toe"

    token2url <- loadToken2URL window
    
    status <- UI.div # set text "Player: X"
    let repeat n = sequence . replicate n
    fields <- repeat 3 $ repeat 3 $
        UI.img # set UI.src (token2url None) # set style [("border","1px solid black")]
    
    getBody window #+ [grid $ map (map element) fields, element status] 


    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
            events <- concat <$> mapM (mapM (event UI.click)) fields
            
            let
                moves :: Event t (Game -> Game)
                moves = foldl1 union $ zipWith (\e s -> move s <$ e) events
                        [(x,y) | y <- [1..3], x <- [1..3]]
                
                state :: Behavior t Game
                state = accumB newGame moves

                currentPlayer :: Behavior t Token
                currentPlayer = player <$> state
                
                tokens :: [Behavior t Token]
                tokens = map (\e -> stepper None (currentPlayer <@ e)) events
        
            zipWithM_
                (\field token -> return field # sink UI.src (token2url <$> token))
                (concat fields)
                tokens
        
            return status # sink text (("Player: " ++) . show <$> currentPlayer)
            
            -- end game event handler
            eState <- changes state
            reactimate $ end window <$> filterJust (isGameEnd . board <$> eState)
    
    network <- compile networkDescription    
    actuate network


end :: Window -> Token -> IO ()
end window result = do
    alert window $
        case result of
            X    -> "X won!"
            O    -> "O won!"
            None -> "Draw!"
    clear window

alert w s = runFunction w (ffi "alert(%1)" s)

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
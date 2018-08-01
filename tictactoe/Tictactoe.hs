-- Author: Eric Kalosa-Kenyon
module Tictactoe where

import Data.Matrix
import Control.Monad

data Player = X | O | E deriving (Show, Read, Eq)
type Board = Matrix Player

-- define initial variables
dim = (3, 3) -- NOTE: This should be treated as hardcoded - the winchecking
            -- methods below rely on 3x3 board

initial_board = fromList (fst dim) (snd dim) (iterate id E)

-- board status calculation subroutines
-- put (w) on (b) at (x, y)
put :: Board -> (Int, Int) -> Player -> Board
put b ix p = setElem p ix b

full :: Board -> Bool
full b = not . any (==E) $ b

-- endgame conditions
-- did player (p) win on board (b)?
won, wonMainDiag, wonAltDiag, wonRows, wonCols :: Player -> Board -> Bool
winconds = [wonMainDiag, wonAltDiag, wonRows, wonCols]
won p b = any (==True) [wincond p b | wincond <- winconds]

wonMainDiag p b = all (==p) (getDiag b)
wonAltDiag p b = ((wonMainDiag p) . (switchCols 1 3)) b
wonRows p b = any (==True) [wonRow p b i | i <- [1..(fst dim)]]
wonCols p b = any (==True) [wonCol p b i | i <- [1..(fst dim)]]

wonRow p b i = all (==p) $ getRow i b
wonCol p b i = all (==p) $ getCol i b

-- user input handling, IO monad stuff
getIndex inp = read inp :: (Int, Int)
getUserIndex = fmap getIndex getLine
boardOutIO = putStrLn . show

-- main workhorse function
playGame b = boardOutIO b >>
    putStrLn "Place your X with format: (Int,Int)" >>
    getUserIndex >>=
    return . (\x -> put b x X) >>= -- returns Monad m :: m Board (IO Board)
    -- TODO: check whether board is valid before updating
    -- TODO: check whether anyone won
    -- return $ (\b -> -- case b of
    --     | won X b   -> strPutLn "X won!"    >> initial_board
    --     | won O b   -> strPutLn "O won!"    >> initial_board
    --     | otherwise ->                      >> b
    -- ) >>=
    playGame

-- main loop
main = playGame initial_board

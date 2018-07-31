module Tictactoe where

import Data.Matrix

data Player = X | O | E deriving (Show, Read, Eq)
-- data Spot = Player | Empty deriving (Eq)
-- data Board = Matrix Spot
type Board = Matrix Player

-- show :: Spot a => a -> String
-- instance Show Spot where
    -- -- show x = if x == Player then show x else "e"
    -- show Empty = "e"

dim = (3, 3) -- NOTE: This should be treated as hardcoded - the winchecking
            -- methods below rely on 3x3 board

-- board = fromList (fst dim) (snd dim) (iterate id Empty)
board = fromList (fst dim) (snd dim) (iterate id E)

-- instance Matrix Board where
--     setElem b

-- put (w) on (b) at (x, y)
put :: Board -> (Int, Int) -> Player -> Board
put b ix p = setElem p ix b

-- did player (p) win on board (b)?
won, wonMainDiag, wonAltDiag, wonRows, wonCols :: Board -> Player -> Bool
won b p = True

-- wonRow b p = or []

wonMainDiag b p = all (==p) (getDiag b)
wonMainDiag b p = all (==p) (getDiag b)

-- TODO
wonAltDiag _ _ = False
wonRows _ _ = False
wonCols _ _ = False

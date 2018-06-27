-- ticTacToe.hs

module TicTacToe where

-- import Data.Char
import Data.List
-- import System.IO

size = 3

type Grid = [[Player]]

data Player = O | N | X deriving (Eq, Show)

next :: Player -> Player
next O = X
next X = O

empty = replicate size (replicate size N)

full :: Grid -> Bool
full grid = N `notElem` concat grid

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)
           ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
             line = all (== p)
             rows = g
             cols = transpose g
             dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g :: wins X g

putGrid :: Grid -> IO ()

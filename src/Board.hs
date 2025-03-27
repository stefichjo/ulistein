{-# LANGUAGE FlexibleInstances #-}
module Board where

import Card (Card (..), top, left, right, bottom, rotations, isValidMatch, Match, Half, allCardPermutations)
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import Data.List (intercalate, find, permutations)
import Data.Array (Array, array, (!), bounds, indices, elems, (//))
import Data.Char (toUpper, toLower)
import Control.Monad (foldM)

type Position = (Int, Int)

type Board = Array Position (Maybe Card)

solutions :: [Board]
solutions = mapMaybe toSolution allCardPermutations

toSolution :: [Card] -> Maybe Board
toSolution = foldM maybeWith emptyBoard

emptyBoard :: Board
emptyBoard = array ((0,0), (2,2)) [((i,j), Nothing) | i <- [0..2], j <- [0..2]]

maybeWith :: Board -> Card -> Maybe Board
board `maybeWith` card =
  if
    null board
  then
    Just (emptyBoard `with` card)
  else
    let
      boards = (board `with`) <$> rotations card
    in
      find isValidBoard boards

with :: Board -> Card -> Board
board `with` card = board // [(nextPosition board, Just card)]

nextPosition :: Board -> Position
nextPosition board = positions !! countCards board
  where
    countCards board = length $ filter isJust (elems board)

    -- Die Reihenfolge, in der Karten platziert werden sollen:
    -- 8 1 5
    -- 4 0 2
    -- 7 3 6
    positions = [(1,1), (0,1), (1,2), (2,1), (1,0), (0,2), (2,2), (2,0), (0,0)]

isValidBoard :: Board -> Bool
isValidBoard = null . getInvalidMatches
  where
    getInvalidMatches board =
      let
        [c0, c1, c2, c3, c4, c5, c6, c7, c8] = elems board

        horizontalNeighbors = [(c0, c1), (c1, c2), (c3, c4), (c4, c5), (c6, c7), (c7, c8)]
        verticalNeighbors = [(c0, c3), (c3, c6), (c1, c4), (c4, c7), (c2, c5), (c5, c8)]

        horizontalMatches = map (getMatch (right, left)) horizontalNeighbors
        verticalMatches = map (getMatch (bottom, top)) verticalNeighbors

        matches = catMaybes (horizontalMatches ++ verticalMatches)
      in
        filter (not . isValidMatch) matches
    getMatch :: (Card -> Half, Card -> Half) -> (Maybe Card, Maybe Card) -> Maybe Match
    getMatch (f, g) (mc1, mc2) = do
      c1 <- mc1
      c2 <- mc2
      return (f c1, g c2)
    
instance {-# OVERLAPPING #-} Show Board where
  show board = unlines $ concatMap showBoardRow [0..2]
    where
      showBoardRow row = [
          intercalate "  " [maybe "  _  " (\c -> "  " ++ [top c]    ++ "  ")                    (board ! (row,col)) | col <- [0..2]],
          intercalate "  " [maybe " _._ " (\c -> " "  ++ [left c]   ++ "." ++ [right c] ++ " ") (board ! (row,col)) | col <- [0..2]],
          intercalate "  " [maybe "  -  " (\c -> "  " ++ [bottom c] ++ "  ")                    (board ! (row,col)) | col <- [0..2]]
        ]

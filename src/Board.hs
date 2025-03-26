{-# LANGUAGE FlexibleInstances #-}
module Board where

import Cards (Card (..), top, left, right, bottom, rotations, isValidMatch, Match, Half, allCardPermutations)
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import Data.List (intercalate, find, permutations)
import Data.Array (Array, array, (!), bounds, indices, elems, (//))
import Data.Char (toUpper, toLower)
import Control.Monad (foldM)

type Position = (Int, Int)

-- Ein Board im Aufbau, mit Maybe für leere Positionen
type Board = Array Position (Maybe Card)

-- Die Reihenfolge, in der Karten platziert werden sollen:
-- 9 2 6
-- 5 1 3
-- 8 4 7
positions :: [Position]
positions = [(1,1), (0,1), (1,2), (2,1), (1,0), (0,2), (2,2), (2,0), (0,0)]

-- | Zeigt ein Board mit allen vier Seiten jeder Karte an
showBoard :: Board -> String
showBoard board = unlines $ concatMap showBoardRow [0..2]
  where
    showBoardRow row = [
        intercalate "  " [maybe "  _  " (\c -> "  " ++ [top c] ++ "  ") (board ! (row,col))    | col <- [0..2]],
        intercalate "  " [maybe " _|_ " (\c -> " " ++ [left c] ++ "|" ++ [right c] ++ " ") (board ! (row,col))  | col <- [0..2]],
        intercalate "  " [maybe "  -  " (\c -> "  " ++ [bottom c] ++ "  ") (board ! (row,col)) | col <- [0..2]]
        ]

-- | Zählt die Anzahl der bereits platzierten Karten
countCards :: Board -> Int
countCards board = length [() | cell <- elems board, isJust cell]

-- Erstellt ein leeres 3x3 Board
emptyBoard :: Board
emptyBoard = array ((0,0), (2,2)) [((i,j), Nothing) | i <- [0..2], j <- [0..2]]

getInvalidMatches :: Board -> [Match]
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
  where
    getMatch :: (Card -> Half, Card -> Half) -> (Maybe Card, Maybe Card) -> Maybe Match
    getMatch (f, g) (mc1, mc2) = do
      c1 <- mc1
      c2 <- mc2
      return (f c1, g c2)

on :: Card -> Board -> Maybe Board
card `on` board =
  if
    null board
  then
    Just (card `strictlyOn` emptyBoard)
  else
    let
      boards = map (`strictlyOn` board) (rotations card)
    in
      find isValidBoard boards
  where
    card `strictlyOn` board =
      let
        cardsCount = length $ filter isJust (elems board)
      in
        board // [(positions !! cardsCount, Just card)]

toSolution :: [Card] -> Maybe Board
toSolution = foldM (flip on) emptyBoard

isValidBoard :: Board -> Bool
isValidBoard = null . getInvalidMatches

solutions :: [Board]
solutions = mapMaybe toSolution allCardPermutations

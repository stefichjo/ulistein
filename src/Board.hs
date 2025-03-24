{-# LANGUAGE FlexibleInstances #-}
module Board where

import Cards (Card (..), top, left, right, bottom, rotations, Half)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.List (intercalate, find)
import Data.Array (Array, array, (!), bounds, indices, elems, (//))
import Data.Char (toUpper, toLower)

type Position = (Int, Int)

-- Ein Board im Aufbau, mit Maybe für leere Positionen
type Board = Array Position (Maybe Card)

type Setter = Card -> Board -> Board

-- Die Reihenfolge, in der Karten platziert werden sollen:
-- 9 2 6
-- 5 1 3
-- 8 4 7
positions :: [Position]
positions = [(1,1), (1,0), (2,1), (1,2), (0,1), (2,0), (2,2), (0,2), (0,0)]

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

on :: Setter
card `on` board =
  let
    cardsCount = length $ filter isJust (elems board)
    place card b = b // [(positions !! cardsCount, Just card)]
  in
    place card board

getNonMatchingHalves :: Board -> [(Half, Half)]
getNonMatchingHalves board =
  let
    [c0, c1, c2, c3, c4, c5, c6, c7, c8] = elems board

    -- Hilfsfunktionen für horizontale und vertikale Paare
    getHorizontalPair :: (Maybe Card, Maybe Card) -> Maybe (Half, Half)
    getHorizontalPair (mc1, mc2) = do
        c1 <- mc1
        c2 <- mc2
        return (right c1, left c2)

    getVerticalPair :: (Maybe Card, Maybe Card) -> Maybe (Half, Half)
    getVerticalPair (mc1, mc2) = do
        c1 <- mc1
        c2 <- mc2
        return (bottom c1, top c2)

    horizontalHalves = map getHorizontalPair [(c0, c1), (c1, c2), (c3, c4), (c4, c5), (c6, c7), (c7, c8)]
    verticalHalves = map getVerticalPair [(c0, c3), (c3, c6), (c1, c4), (c4, c7), (c2, c5), (c5, c8)]

    halves = horizontalHalves ++ verticalHalves
  in
    filter (not . matches) $ catMaybes halves

maybeOn :: Card -> Board -> Maybe Board
maybeOn card board =
  if
    null board
  then
    Just (card `on` emptyBoard)
  else
    let
      boards = map (`on` board) (rotations card)
    in
      find isValid boards

matches :: (Half, Half) -> Bool
matches ('s', 'S') = True
matches ('S', 's') = True
matches ('k', 'K') = True
matches ('K', 'k') = True
matches ('m', 'M') = True
matches ('M', 'm') = True
matches ('p', 'P') = True
matches ('P', 'p') = True
matches _ = False

isValid :: Board -> Bool
isValid board = null $ getNonMatchingHalves board

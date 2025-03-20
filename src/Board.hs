{-# LANGUAGE FlexibleInstances #-}
module Board where

import Cards (Card, rotateCard, top, left, right, bottom)
import Data.Maybe (isJust)
import Data.List (intercalate)
import Data.Array (Array, array, (!), bounds, indices, elems, (//))

type Position = (Int, Int)

-- Die Reihenfolge, in der Karten platziert werden sollen:
-- 9 2 6
-- 5 1 3
-- 8 4 7
positions :: [Position]
positions = [(1,1), (1,0), (2,1), (1,2), (0,1), (2,0), (2,2), (0,2), (0,0)]

-- Ein Board im Aufbau, mit Maybe für leere Positionen
type Board = Array Position (Maybe Card)

-- Ein möglicherweise ungültiges Board
type MaybeBoard = Maybe Board

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

on :: Card -> Board -> Board
card `on` board =
  let
    cardsCount = length $ filter isJust (elems board)
    setter card b = b // [(positions !! cardsCount, Just card)]
  in
    setter card board

type Setter = Card -> Board -> Board


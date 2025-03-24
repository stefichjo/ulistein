{-# LANGUAGE FlexibleInstances #-}
module Board where

import Cards (Card, top, left, right, bottom, rotationsCard, Half)
import Data.Maybe (isJust, isNothing)
import Data.List (intercalate)
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
    setter card b = b // [(positions !! cardsCount, Just card)]
  in
    setter card board


maybeOn :: Card -> Board -> Maybe Board
maybeOn card board =
  let
    rotations = rotationsCard card
    boards = map (`on` board) rotations
  in
    undefined

isValid :: Board -> Bool
isValid board =
  let
    cardsCount = countCards board
  in
    undefined

adjacentHalves :: Board -> [(Half, Half)]
adjacentHalves board =
  let
    -- Positions of horizontally adjacent cards
    horizontalPairs = [(row,col) | row <- [0..2], col <- [0..1]]
    -- Positions of vertically adjacent cards
    verticalPairs = [(row,col) | row <- [0..1], col <- [0..2]]
    
    -- Get cards at adjacent positions, if they exist
    horizontalCards = [(c1, c2) | pos <- horizontalPairs, 
                                 Just c1 <- [board ! pos],
                                 Just c2 <- [board ! (fst pos, snd pos + 1)]]
    verticalCards = [(c1, c2) | pos <- verticalPairs,
                               Just c1 <- [board ! pos],
                               Just c2 <- [board ! (fst pos + 1, snd pos)]]
    
    -- Extract matching sides from adjacent cards
    rowAdjacents = [(right c1, left c2) | (c1, c2) <- horizontalCards]
    colAdjacents = [(bottom c1, top c2) | (c1, c2) <- verticalCards]
  in
    rowAdjacents ++ colAdjacents

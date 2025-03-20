{-# LANGUAGE FlexibleInstances #-}
module Board where

import Cards (Card, rotateCard, top, left, right, bottom)
import Data.Maybe (isJust)
import Data.List (intercalate)

-- Ein Board im Aufbau, mit Maybe für leere Positionen
type Board = [[Maybe Card]]

-- Ein möglicherweise ungültiges Board
type MaybeBoard = Maybe Board

-- | Zeigt ein Board mit allen vier Seiten jeder Karte an
showBoard :: Board -> String
showBoard board = unlines $ concatMap showBoardRow board  -- Board nicht mehr umdrehen
  where
    showBoardRow row = [
        intercalate "  " [maybe "  _  " (\c -> "  " ++ [top c] ++ "  ") cell    | cell <- row],
        intercalate "  " [maybe " _|_ " (\c -> " " ++ [left c] ++ "|" ++ [right c] ++ " ") cell  | cell <- row],
        intercalate "  " [maybe "  -  " (\c -> "  " ++ [bottom c] ++ "  ") cell | cell <- row]
        ]

-- Die Reihenfolge, in der Karten platziert werden sollen:
-- 9 2 6
-- 5 1 3
-- 8 4 7
positions :: [(Int, Int)]
positions = [(1,1), (1,0), (2,1), (1,2), (0,1), (2,0), (2,2), (0,0), (0,2)]

-- | Zählt die Anzahl der bereits platzierten Karten
countCards :: Board -> Int
countCards board = length [ () | row <- board, Just _ <- row ]

-- Erstellt ein leeres 3x3 Board
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

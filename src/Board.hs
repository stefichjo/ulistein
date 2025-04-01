{-# LANGUAGE FlexibleInstances #-}
module Board where

import Card (Card (..), top, left, right, bottom, rotations, isValidMatch, Match, Half, allCardPermutations, allCards)
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import Data.List (intercalate, find, permutations, transpose, elemIndex)
import Data.Array (Array, array, (!), bounds, indices, elems, (//))
import Utils (chunksOf)
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
    positions = [(1,1), (0,1), (1,2), (2,1), (1,0), (0,2), (2,2), (2,0), (0,0)]

isValidBoard :: Board -> Bool
isValidBoard = null . getInvalidMatches
  where
    getInvalidMatches board =
      let
        [c0, c1, c2, c3, c4, c5, c6, c7, c8] = elems board

        horizontalMatches = mapMaybe (getMatch (right, left)) [
            (c0, c1), (c1, c2),
            (c3, c4), (c4, c5),
            (c6, c7), (c7, c8)
          ]
        verticalMatches = mapMaybe (getMatch (bottom, top)) [
            (c0, c3), (c1, c4), (c2, c5),
            (c3, c6), (c4, c7), (c5, c8)
          ]

        matches = horizontalMatches ++ verticalMatches
      in
        filter (not . isValidMatch) matches
    getMatch :: (Card -> Half, Card -> Half) -> (Maybe Card, Maybe Card) -> Maybe Match
    getMatch (f, g) (mc1, mc2) = do
      c1 <- mc1
      c2 <- mc2
      return (f c1, g c2)

instance {-# OVERLAPPING #-} Show (Maybe Card) where
  show Nothing = unlines [
      " _ ",
      "_._",
      " _ "
    ]
  show (Just card) = unlines [
      " " ++ show (top card) ++ " ",
      show (left card) ++ cardIndex card ++ show (right card),
      " " ++ show (bottom card) ++ " "
    ]

cardIndex :: Card -> String
cardIndex card = maybe "." show (elemIndex (Just card) (map Just allCards))

instance {-# OVERLAPPING #-} Show Board where
  show board =
    let
      cardLines = map (lines . show) (elems board)
      groupedLines = map (transpose . take 3) (chunksOf 3 cardLines)
    in
      unlines $ concatMap (map unwords) groupedLines

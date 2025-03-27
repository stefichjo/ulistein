module Card where

import Data.Maybe (isJust)
import Data.List (permutations)

-- Eine Tierhälfte ist ein Buchstabe: Großbuchstabe = Upper, Kleinbuchstabe = Lower
type Half = Char

type Match = (Half, Half)

-- Eine Karte hat vier Tierhälften (oben, rechts, unten, links)
data Card = Card {
    top    :: Half,
    right  :: Half,
    bottom :: Half,
    left   :: Half
} deriving (Eq)

allCards :: [Card]
allCards = [
    Card 'S' 'k' 'M' 'p', -- 0
    Card 'k' 'S' 'M' 'p', -- 1
    Card 'k' 'p' 'S' 'M', -- 2
    Card 'm' 'k' 'P' 'S', -- 3
    Card 'K' 'M' 'S' 's', -- 4
    Card 'K' 's' 'P' 'm', -- 5
    Card 'p' 's' 'K' 'm', -- 6
    Card 'm' 's' 'P' 'K', -- 7
    Card 'k' 'M' 's' 'P'  -- 8
  ]

allCardPermutations :: [[Card]]
allCardPermutations = permutations allCards

instance Show Card where
  show (Card t r b l) = [t, r, b, l]

rotations :: Card -> [Card]
rotations card = take 4 $ iterate rotate card

rotate :: Card -> Card
rotate (Card t r b l) = Card l t r b

isValidMatch :: Match -> Bool
isValidMatch = (`elem` [
    ('s', 'S'), ('S', 's'),
    ('k', 'K'), ('K', 'k'),
    ('m', 'M'), ('M', 'm'),
    ('p', 'P'), ('P', 'p')
  ])

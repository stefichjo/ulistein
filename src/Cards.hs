{-# LANGUAGE InstanceSigs #-}
module Cards where

import Data.Maybe (isJust)

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

instance Show Card where
    show :: Card -> String
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


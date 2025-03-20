{-# LANGUAGE InstanceSigs #-}
module Cards where

import Data.Maybe (isJust)

-- Eine Tierhälfte ist ein Buchstabe: Großbuchstabe = Upper, Kleinbuchstabe = Lower
type Half = Char

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

-- | Rotiert eine Karte n-mal nach rechts
rotateCard :: Card -> Int -> Card
rotateCard card 0 = card
rotateCard (Card t r b l) n = rotateCard (Card l t r b) (n - 1)

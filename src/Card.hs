{-# LANGUAGE FlexibleInstances #-}
module Card (Card(..), Half(..), Part(..), Animal(..), rotate, rotations, isValidMatch, Match, allCardPermutations, allCards) where

import Data.Maybe (isJust)
import Data.List (permutations)

-- Eine Tierhälfte ist ein Buchstabe: Großbuchstabe = Upper, Kleinbuchstabe = Lower
data Part = Upper | Lower deriving (Eq)
data Animal = Schwein | Pinguin | Katze | Maus deriving (Eq)

data Half = Half Part Animal deriving (Eq)

upper :: Animal -> Half
upper = Half Upper

lower :: Animal -> Half
lower = Half Lower

type Match = (Half, Half)

-- Eine Karte hat vier Tierhälften (oben, rechts, unten, links)
data Card = Card {
    top    :: Half,
    right  :: Half,
    bottom :: Half,
    left   :: Half
} deriving (Eq)

instance Show Half where
  show (Half Upper Schwein) = "S"
  show (Half Lower Schwein) = "s"
  show (Half Upper Pinguin) = "P"
  show (Half Lower Pinguin) = "p"
  show (Half Upper Katze) = "K"
  show (Half Lower Katze) = "k"
  show (Half Upper Maus) = "M"
  show (Half Lower Maus) = "m"

instance Show Card where
  show (Card t r b l) = show t ++ show r ++ show b ++ show l

allCards :: [Card]
allCards = [
    Card (upper Schwein) (lower Katze) (upper Maus) (lower Pinguin), -- 0
    Card (lower Katze) (upper Schwein) (upper Maus) (lower Pinguin), -- 1
    Card (lower Katze) (lower Pinguin) (upper Schwein) (upper Maus), -- 2
    Card (lower Maus) (lower Katze) (upper Pinguin) (upper Schwein), -- 3
    Card (upper Katze) (upper Maus) (upper Schwein) (lower Schwein), -- 4
    Card (upper Katze) (lower Schwein) (upper Pinguin) (lower Maus), -- 5
    Card (lower Pinguin) (lower Schwein) (upper Katze) (lower Maus), -- 6
    Card (lower Maus) (lower Schwein) (upper Pinguin) (upper Katze), -- 7
    Card (lower Katze) (upper Maus) (lower Schwein) (upper Pinguin)  -- 8
  ]

allCardPermutations :: [[Card]]
allCardPermutations = permutations allCards

instance {-# OVERLAPPING #-} Eq (Maybe Card) where
  Nothing == Nothing = True
  Nothing == _ = False
  _ == Nothing = False
  Just c1 == Just c2 = c1 `elem` rotations c2

rotations :: Card -> [Card]
rotations card = take 4 $ iterate rotate card

rotate :: Card -> Card
rotate (Card t r b l) = Card l t r b

isValidMatch :: Match -> Bool
isValidMatch (Half p1 a1, Half p2 a2) =
  a1 == a2 && p1 /= p2

{-# LANGUAGE FlexibleInstances #-}
module Card (Card(..), Half(..), Part(..), Animal(..), rotate, rotations, isValidMatch, Match, allCardPermutations, allCards) where

import Data.Maybe (isJust)
import Data.List (permutations)

-- Eine Tierhälfte ist ein Buchstabe: Großbuchstabe = Upper, Kleinbuchstabe = Lower
data Part = Upper | Lower deriving (Eq)
data Animal = Schwein | Pinguin | Katze | Maus deriving (Eq)

data Half = Half Part Animal deriving (Eq)

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
  show (Half Lower Pinguin) = "p"
  show (Half Upper Pinguin) = "P"
  show (Half Upper Katze) = "K"
  show (Half Lower Katze) = "k"
  show (Half Upper Maus) = "M"
  show (Half Lower Maus) = "m"

instance Show Card where
  show (Card t r b l) = show t ++ show r ++ show b ++ show l

allCards :: [Card]
allCards = [
    Card (Half Upper Schwein) (Half Lower Katze) (Half Upper Maus) (Half Lower Pinguin), -- 0
    Card (Half Lower Katze) (Half Upper Schwein) (Half Upper Maus) (Half Lower Pinguin), -- 1
    Card (Half Lower Katze) (Half Lower Pinguin) (Half Upper Schwein) (Half Upper Maus), -- 2
    Card (Half Lower Maus) (Half Lower Katze) (Half Upper Pinguin) (Half Upper Schwein), -- 3
    Card (Half Upper Katze) (Half Upper Maus) (Half Upper Schwein) (Half Lower Schwein), -- 4
    Card (Half Upper Katze) (Half Lower Schwein) (Half Upper Pinguin) (Half Lower Maus), -- 5
    Card (Half Lower Pinguin) (Half Lower Schwein) (Half Upper Katze) (Half Lower Maus), -- 6
    Card (Half Lower Maus) (Half Lower Schwein) (Half Upper Pinguin) (Half Upper Katze), -- 7
    Card (Half Lower Katze) (Half Upper Maus) (Half Lower Schwein) (Half Upper Pinguin)  -- 8
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
isValidMatch = (`elem` [
    (Half Lower Schwein, Half Upper Schwein), (Half Upper Schwein, Half Lower Schwein),
    (Half Lower Katze, Half Upper Katze), (Half Upper Katze, Half Lower Katze),
    (Half Lower Maus, Half Upper Maus), (Half Upper Maus, Half Lower Maus),
    (Half Lower Pinguin, Half Upper Pinguin), (Half Upper Pinguin, Half Lower Pinguin)
  ])

module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Board ( emptyBoard, Board )
import Cards (Card(..))
import Data.Maybe (isNothing)

spec :: Spec
spec = do
  describe "Board" $ do
    it "sollte ein leeres 3x3 Board erstellen" $ do
      let board = emptyBoard
      length board `shouldBe` 3  -- 3 Zeilen
      all ((== 3) . length) board `shouldBe` True  -- 3 Spalten
      all (all isNothing) board `shouldBe` True  -- Alle Felder leer

    it "sollte die erste Karte unrotiert in die Mitte des Boards legen" $ do
      let firstCard = Card 's' 'p' 'm' 'k'
      let boardWithFirstCard = [
              [Nothing, Nothing, Nothing],
              [Nothing, Just firstCard, Nothing],
              [Nothing, Nothing, Nothing]
            ]
      (emptyBoard `with` firstCard) `shouldBe` boardWithFirstCard

with :: Board -> Card -> Board
with [[a0, a1, a2], [b0, _, b2], [c0, c1, c2]] card = [
    [a0, a1, a2],
    [b0, Just card, b2],
    [c0, c1, c2]
  ]

-- TODO: list of selectors, instead of list of positions
-- TODO: coordinates -> selector
-- TODO: use arrays instead of lists of lists


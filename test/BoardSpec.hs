module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Cards (Card(..))
import Data.Maybe (isNothing, isJust)
import Data.Array ((!), array, elems, (//))
import Board

spec :: Spec
spec = do
  describe "Board" $ do
    it "sollte ein leeres 3x3 Board erstellen" $ do
      all isNothing (elems emptyBoard) `shouldBe` True  -- Alle Felder leer

    it "sollte die erste Karte unrotiert in die Mitte des Boards legen" $ do
      let
        firstCard = Card 's' 'p' 'm' 'k'
      firstCard `on` emptyBoard `shouldBe` emptyBoard // [((1,1), Just firstCard)]

    it "sollte die zweite Karte auf das Board platzieren" $ do
      let
        previousBoard = Card 's' 'p' 'm' 'k' `on` emptyBoard
        nextCard = Card 'S' 'P' 'M' 'K'
        nextBoard = previousBoard // [((1, 0), Just nextCard)]
      nextCard `on` previousBoard `shouldBe` nextBoard

